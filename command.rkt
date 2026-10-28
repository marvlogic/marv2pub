#lang racket

(require
  racket/cmdline
  racket/pretty
  racket/string
  racket/path
  racket/contract

  marv/state
  marv/logger
  marv/resources
  marv/resolver
  )

(log-marv-info "** HELLO FROM MARV **")

; TODO - don't need this many parameters to do everything
(define APPLY (make-parameter #f))
(define PURGE (make-parameter #f))
(define SHOW-STATE-IDS (make-parameter #f))
(define DUMP-RESOURCES (make-parameter #f))
(define LIST-RESOURCES (make-parameter #f))
(define REFRESH-STATE (make-parameter #f))
(define IMPORT-RESOURCES (make-parameter #f))
(define DIFF-STATE-REMOTE (make-parameter #f))
(define DIFF-SPEC-STATE (make-parameter #f))
(define PLAN-CHANGES (make-parameter #f))
(define STATE-RM (make-parameter #f))
(define LIST-PARAMS (make-parameter #f))
(define PARAMS (make-parameter (hash)))

(define STATE-FILE (make-parameter #f))
(define SHOW-DOCS (make-parameter #f))
(define USE-RESOLVER (make-parameter #f))

(log-level-set 'warn)
(log-filter-set "(.*.rkt)")

(define RESOURCES
  (command-line
   #:once-each
   [("-s" "--state") state-file "Name of statefile to use" (STATE-FILE state-file)]
   [("--purge")  "Purge (DELETE) all resources" (PURGE #t)]
   [("--resolver") "Use interactive resolver if failed" (USE-RESOLVER #f)]
   [("--log-level") log-level "Set log-level" (log-level-set (string->symbol log-level))]
   [("--log-filter") log-filter "Set log-filter regexp"
                     (log-filter-set log-filter)]

   #:once-any
   ;; [("--docs") type "Show documentation for a type e.g gcp:compute.network" (SHOW-DOCS (string->symbol type))]
   [("--plan")  "Plan changes" (PLAN-CHANGES #t)]
   [("--apply")  "Apply resources" (APPLY #t)]
   [("--list") "Show the defined resources" (LIST-RESOURCES #t)]
   [("--state-ids") "List resource IDs from state" (SHOW-STATE-IDS #t)]
   [("--dump") "Dump full output of resources from state" (DUMP-RESOURCES #t)]
   ;; [("--refresh") "Refresh the state" (REFRESH-STATE #t)]
   ;; [("--import") ids "Import the already existing resource IDs into state" (IMPORT-RESOURCES ids)]
   ;; [("--remote-diff") "Report difference between STATE & remote" (DIFF-STATE-REMOTE #t)]
   [("--state-rm") id "Remove item from state" (STATE-RM id)]
   ;; [("--list-params") "Lists parameters accepted by module" (LIST-PARAMS #t)]

   #:multi
   [("--param") param value "Set <param> to <value>" (PARAMS (hash-set (PARAMS) (string->symbol param) value))]

   #:args (module-file) module-file))

;; (log-level-set 'debug)
;;(log-filter-set "(.*boto.*|command|state|tasks|.*barrier.*).rkt")
;; (log-filter-set "(.*.rkt)")

(when (not (STATE-FILE))
  (STATE-FILE (path-replace-extension (file-name-from-path (string->path RESOURCES)) ".state.dat" )))

(printf "Using state file: ~a\n" (STATE-FILE))
(state-load (STATE-FILE))
(plan-init)

(define top-main (lambda any-params (hash)))

(define/contract (load-module f)
  (string? . -> . void?)
  (define rel-mod (find-relative-path (current-directory) f) )
  (set! top-main (dynamic-require rel-mod 'main))
  (void))

(define (st-wrapper thk)
  (with-handlers
      ([exn:fail?
        (lambda(e)
          (displayln (~a "Error during wrapper:" e ))
          (define cms (exn-continuation-marks e))
          (define context (continuation-mark-set->context cms))
          (for-each (lambda (frame) (printf "~a\n" frame)) context)
          (state-save "state-int.dat")
          (exit 1))])
    (thk)))

(load-module RESOURCES)
(cond
  ;; [(SHOW-STATE-IDS)
  ;;  (pretty-print (state-keys))]

  ;; [(SHOW-DOCS)
  ;;  (display-docs (SHOW-DOCS))
  ;; (exit 0)]

  ; [(DIFF-STATE-REMOTE)
  ;  (define (do-diff k v)
  ;    ; TODO: will fail if remote is removed
  ;    (define remote-state (drv-read #:meta k #:resource v))
  ;    (pretty-display(make-diff v remote-state)))
  ;  (state-for-each do-diff)]

  ; [(REFRESH-STATE)
  ;  (define (update k)
  ;    (state-set-ref
  ;     k
  ;     (drv-read #:meta k
  ;               #:resource (state-ref k) )))
  ;  (map update (state-keys))
  ;  ]

  ;; [(IMPORT-RESOURCES)
  ;;  (import-resources modl (map string->symbol (string-split (IMPORT-RESOURCES) ",")))]

  [(DUMP-RESOURCES)
   (pretty-print(state-repr))]

  ;; [(LIST-RESOURCES)
  ;;  (pretty-print  modl)]

  [(PLAN-CHANGES)
   (top-main (PURGE))
   (plan-display)
   ;; (define plan (top-main))
   #;(output-plan plan)]

  [(APPLY)
   (top-main (PURGE))
   (call-with-exception-handler
    (lambda(e)
      (printf "\nERROR: ~a\n" e)
      (state-save (STATE-FILE))
      (define cms (exn-continuation-marks e))
      (define context (continuation-mark-set->context cms))
      (for-each (lambda (frame) (printf "~a\n" frame)) context)
      (exit 1))
    (lambda()
      (define report (plan-apply))
      (define failed-plan-items (hash-keys (report-filter report 'failed)))
      (cond
        [(null? failed-plan-items)
         (displayln "** SUCCESS")]
        [(USE-RESOLVER)
         (displayln "** FAILED: ENTERING INTERACTIVE RESOLVER **")
         (resolver-interact report)
         (displayln "Resolver closed")]
        [else
         (displayln "** FAILED: **")
         (displayln failed-plan-items)
         ])
      ))]

  ;; [(STATE-RM)
  ;;  (define ids (map string->symbol (string-split (STATE-RM) ",")))
  ;;  (map (lambda (id) (printf "deleting ~a" id) (state-delete id)) ids)]


  #;[(LIST-PARAMS)
     (define params (get-resource-params))
     (displayln (params-accepted params))
     (displayln (params-required params))
     ]
  )

(cond
  [(or (IMPORT-RESOURCES) (REFRESH-STATE) (APPLY) (STATE-RM))
   (log-marv-debug "Saving state")
   (state-save (STATE-FILE)) ])
