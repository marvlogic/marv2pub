#lang racket

(provide
 types-lookup
 docker-image
 docker-container
 docker-job
 )

(require
  json
  marv/docker/cmds
  marv/stx-utils
  marv/model
  marv/utils
  marv/resources
  marv/logger
  (only-in marv/state state-ref state-in?))

(define types-lookup (make-hash))
(define-syntax (define/hdlr stx)
  (syntax-case stx ()
    ([_ ...] (define-lookup-hdlr #'types-lookup stx))))

(define (mk-comp-resource type id drvr cfg [check? #f])

  ;; Creates a new resource, compositing the shape's blueprint with the given configuration. Returns a
  ;; computation struct containing the merged config and computation-shape

  ;; TODO: convert to using a model as input, once all resource models are ready

  ;; (check-io-blueprint request-shp-id mod-request-shp-id result-shp-id cfg check?)

  ;; FIXME: !! the state-ref in here is a smell. There may be a better method to pass in existing
  ;; configuration from current state, if it exists, rather than setting up all the computations

  (define (safe-state-ref) (if (state-in? id) (computation-hs (resource-cfg (state-ref id))) 'undef))

  (define model
    (case type
      [(docker-image) image-model]
      [(docker-container) container-model]
      [(docker-job) job-model]
      [else (error 'docker "unsupported docker resource:~a" type)]))

  (resource
   type id
   (driver-id drvr)
   (computation
    (hash-merge<
     cfg
     ((mk-blueprint-fn model) safe-state-ref)))))

(define image-model
  (mkih
   name 'immutable
   build-path 'immutable))

(define (((docker-image cfg)drvr)id)
  (mk-comp-resource 'docker-image id drvr cfg))

(define/hdlr (docker-image cmd)
  (log-marv-debug (~a "docker-image " cmd))
  (match cmd
    [(op-create type cfg)
     (docker-build #:name (hash-ref cfg 'name)
                   #:content (hash-ref cfg 'build-path))
     cfg]
    [(op-read _ cfg) cfg]
    [(op-diff _ oldcfg newcfg)
     (if (equal? oldcfg newcfg) #f (op-diff-result (hash) #t))]
    [(op-delete _ cfg)
     (docker-image-remove #:name (hash-ref cfg 'name))]
    [_ (log-marv-debug (~a "ignoring unknown command:" cmd))
       cmd]))

(define container-model
  (mkih
   name 'immutable
   image-name 'immutable
   entrypoint 'immutable
   container-id 'ready-only
   ))

(define (((docker-container cfg)drvr)id)
  (mk-comp-resource 'docker-container id drvr cfg))

(define/hdlr (docker-container cmd)
  (log-marv-debug (~a "docker-container " cmd))
  (match cmd
    [(op-create type cfg)
     (log-marv-debug cfg)
     (define entrypoint (hash-ref cfg 'entrypoint #f))
     (log-marv-debug entrypoint)
     (define cid (docker-create #:name (hash-ref cfg 'name)
                                #:image-name (hash-ref cfg 'image-name)
                                #:entrypoint entrypoint))
     (hash-set cfg 'container-id cid)]
    [(op-read _ cfg) cfg]
    [(op-diff _ oldcfg newcfg) #f]
    [(op-delete _ cfg)
     (docker-remove #:name (hash-ref cfg 'container-id))]
    [_ (log-marv-debug (~a "ignoring unknown command:" cmd))
       cmd]))

(define job-model
  (mkih
   name 'immutable
   image-name 'immutable
   command 'immutable
   log 'immutable
   container-id 'read-only
   exit-status 'read-only
   ))

(define (((docker-job cfg)drvr)id)
  (mk-comp-resource 'docker-job id drvr cfg))

(define (docker-container-state id)
  (hash-ref (car (string->jsexpr (docker-inspect #:name id))) 'State))

(define (create-start-job-container name image-name envvars input-files cmd-str)
  (define id
    (docker-create
     #:name name
     #:entrypoint "/marv.sh"
     #:envvars envvars
     ;;#:replace? #t
     #:image-name image-name))
  (define script-file (path->string (make-temporary-file)))
  (with-output-to-file #:mode 'text #:exists 'replace
    script-file (lambda() (displayln cmd-str)))
  (file-or-directory-permissions script-file user-permission-bits)
  (for ([f (cons (list script-file "/marv.sh") input-files)])
    (docker-copy #:name id #:src (car f) #:dest (format "~a:~a" id (cadr f))))
  (docker-start #:name id)
  id)

(define (start-job cfg)
  (define cid (create-start-job-container
               (hash-ref cfg 'name)
               (hash-ref cfg 'image-name)
               (hash-ref cfg 'environment (hash))
               (hash-ref cfg 'input-files '())
               (hash-ref cfg 'command #f)))
  (log-marv-debug (~a "started container id: " cid))
  (hash-set* cfg
             'container-available #t
             'container-id cid
             'exit-status #f))

(define (finish-job cfg)
  (log-marv-debug (~a "finishing docker job " (hash-ref cfg 'container-id)))
  (define cid (hash-ref cfg 'container-id))
  (define logcfg (when-undef? (safe-hash-ref cfg 'log) 'on-error))
  (define logfile
    (let ([fn (hash-ref cfg 'logfile (format "~a-{id}.log" (hash-ref cfg 'name)))])
      (if fn (string-replace fn "{id}" (substring cid 0 8)) fn)))
  (define err (hash-ref (hash-ref cfg 'state) 'ExitCode))
  (define result (hash-set* cfg 'container-available #f 'exit-status err 'logfile logfile))
  (when (or (eq? logcfg 'always) (and (eq? logcfg 'on-error) (> err 0)))
    (docker-logs #:name cid #:filename logfile))
  (docker-remove #:name cid)
  result
  #;(cond
      [(eq? err 0) (docker-remove #:name cid) result]
      [else result (error 'docker "docker job ~a failed: ~a" cid cfg)]))

(define/hdlr (docker-job cmd)
  (log-marv-debug (~a "docker-job " cmd))
  (match cmd
    [(op-create _ cfg) (start-job cfg) ]
    [(op-read _ cfg)
     (cond
       [(hash-ref cfg 'container-available #f)
        (log-marv-debug (~a "refreshing " (hash-ref cfg 'container-id)))
        (define ns (docker-container-state (hash-ref cfg 'container-id)))
        (define newcfg (hash-set cfg 'state ns))
        (cond
          [(hash-ref ns 'Running) newcfg]
          [else (finish-job newcfg)])]
       [else cfg]) ]
    [(op-diff _ _ _) (op-diff-result #t #t) ]
    [(op-delete _ cfg) #t]
    [_ (log-marv-warn (~a "ignoring unknown command:" cmd))
       cmd]))

