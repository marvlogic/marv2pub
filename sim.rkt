#lang racket

(provide
 sim-init
 sim-persist-state
 sim-resource-types
 sim-resources
 alpha
 beta
 delta
 epsilon
 gamma
 sim-driver)

(require
  racket/generator
  marv/resources
  marv/logger
  marv/model
  marv/utils
  (only-in marv/state state-dump state-ref state-in?)
  (for-syntax syntax/parse))

;; Resource simulator

(define-syntax (mkih stx)
  (syntax-parse stx
    [(_ (~seq a b) ...) #'(hash (~@ 'a b) ...) ]))


(define sim-resources (make-hash))
(define i1-xrefs (make-hash))
(define id-gen (sequence->generator (in-naturals)))

(define (sim-init)
  (set! sim-resources (make-hash))
  (set! i1-xrefs (make-hash)))

(define (sim-save-state f)
  (define sim-out
    (hash 'resources sim-resources
          'xrefs i1-xrefs))
  (with-output-to-file #:exists 'replace f
    (lambda() (pretty-write (hash->list sim-out)))))

(define (sim-load-state f)
  (define sim-in
    (with-input-from-file f (lambda()  (make-hash (read)))))
  (set! sim-resources (make-hash (hash->list (hash-ref sim-in 'resources))))
  (set! i1-xrefs (make-hash (hash->list (hash-ref sim-in 'xrefs))))
  (set! id-gen (sequence->generator (in-naturals (add1 (hash-count sim-resources))))))

(define (sim-persist-state f [initial-load #t])
  (when (and initial-load (file-exists? f)) (sim-load-state f))
  (plumber-add-flush! (current-plumber) (lambda(_)(sim-save-state f))))

(define (generic-create type cfg)
  (define new-id (format "id-~a~a" type (id-gen)))
  (define newcfg (hash-set* cfg 'id new-id 'ro1 0))
  (hash-set! sim-resources new-id newcfg)
  newcfg)

(define (generic-read cfg) (hash-ref sim-resources (hash-ref cfg 'id)))

(define (generic-update cfg diff)
  (hash-set! sim-resources (hash-ref cfg 'id) cfg)
  cfg)

(define (generic-delete cfg)
  (define rid (hash-ref cfg 'id))
  (when (hash-has-key? i1-xrefs rid)
    (state-dump)
    (raise
     (format "failed to remove ~a; has dependent(s): ~a" rid (hash-ref i1-xrefs rid))))
  (hash-remove! sim-resources rid))

(define (mk-computation-shape rootfn)
  (define (safe-hash-ref hs k) (if (undef? hs) 'undef (hash-ref hs k 'undef)))
  (mkih
   id (lambda()(safe-hash-ref (rootfn) 'id))
   m1 (lambda()(safe-hash-ref (rootfn) 'm1))
   i1 (lambda()(safe-hash-ref (rootfn) 'i1))
   ro1 (lambda()(safe-hash-ref (rootfn) 'ro1))))

(define (mk-comp-resource type id drvr cfg)
  ;; TODO: same smell here about using state-ref et al
  (define (safe-state-ref) (if (state-in? id) (computation-hs (resource-cfg (state-ref id))) 'undef))
  (resource
   type id
   (driver-id drvr)
   (computation
    (hash-merge< cfg (mk-computation-shape safe-state-ref)))))

(define std-model
  (hash->model-attrs
   (mkih
    id 'read-only
    m1 'mutable
    m2 'mutable
    i1 'immutable
    i2 'immutable
    ro1 'read-only)))

(define (mk-generic-hdlr id model)
  (lambda (cmd)
    (log-marv-debug (format "gen:~a ~a" id cmd))
    (match cmd
      [(op-create type cfg) (generic-create type cfg)]
      [(op-read _ cfg) (generic-read cfg)]
      [(op-update _ cfg diff) (generic-update cfg diff)]
      [(op-delete _ cfg) (generic-delete cfg)]
      [(op-diff _ oldcfg newcfg) (generic-diff model oldcfg newcfg)]
      [_ (log-marv-debug (~a "ignoring unknown command:" cmd))])))

(define (mk-generic-type type)
  (curry (lambda (cfg drvr id) (mk-comp-resource type id drvr cfg))))

(define alpha-model std-model)
(define beta-model std-model)
(define delta-model std-model)
(define epsilon-model std-model)
(define gamma-model std-model)

(define alpha (mk-generic-type 'alpha))
(define beta (mk-generic-type 'beta))
(define delta (mk-generic-type 'delta))
(define epsilon (mk-generic-type 'epsilon))
(define gamma (mk-generic-type 'gamma))

(define (create-with-gamma-ref type cfg)
  (define g-ref (hash-ref cfg 'i1 'xxx))
  (cond
    [(hash-has-key? sim-resources g-ref)
     (define newg (generic-create type cfg))
     (define id (hash-ref newg 'id))
     (hash-set! i1-xrefs g-ref (cons id (hash-ref i1-xrefs g-ref '())))
     newg]
    [else (raise (~a "i1 must refer to id of an existing sim resource: " cfg))]))

(define (delete-gamma-ref cfg)
  (define g-ref (hash-ref cfg 'i1))
  (define rm (remove (hash-ref cfg 'id) (hash-ref i1-xrefs g-ref)))
  (if (null? rm)
      (hash-remove! i1-xrefs g-ref)
      (hash-set! i1-xrefs g-ref rm)) )

(define (delta-hdlr cmd)

  ;; delta resources have a ro1 field that is incremented every time the resource is read (for
  ;; simulating state changes). They must also refer to an existing resource via i1, which is recorded
  ;; in i1-xrefs. When deleting, they persist until 3 op-reads (counted down by ro1) then the
  ;; status field is set to 'deleted but the resource is never cleaned out of the simulator (similar
  ;; to AWS instance behaviour)

  (log-marv-debug (format "delta ~a"  cmd))
  (define gen-hdlr (mk-generic-hdlr 'delta delta-model))
  (define (deleting? c)(hash-ref c 'deleting #f))
  (match cmd
    [(op-create type cfg) (create-with-gamma-ref type cfg)]
    [(op-read _ cfg)
     (define sim-cfg (generic-read cfg))
     (log-marv-debug (~a "sim state:" sim-cfg))
     (define ro1 (hash-ref sim-cfg 'ro1))
     (cond
       [(and (deleting? sim-cfg) (> ro1 0))
        (generic-update (hash-set sim-cfg 'ro1 (sub1 ro1)) null)
        (generic-read sim-cfg) ]
       [(deleting? sim-cfg)
        (log-marv-debug "reached hard delete")
        (delete-gamma-ref sim-cfg)
        (generic-update (hash-set sim-cfg 'deleted #t) null)
        ]
       [else
        (generic-update (hash-set sim-cfg 'ro1 (add1 ro1)) null)
        (generic-read sim-cfg)])]
    [(op-delete _ cfg)
     (define id (hash-ref cfg 'id))
     (define sim-cfg (hash-ref sim-resources id))
     (generic-update (hash-set* sim-cfg 'deleting #t 'ro1 3) null)
     (generic-read sim-cfg)]
    [_ (gen-hdlr cmd)]))

(define (epsilon-hdlr cmd )

  ;; epsilon (error) resource will fail to create or delete depending on the value
  ;; of field i1: 'fail-on-create or 'fail-on-delete
  ;;

  (log-marv-debug (format "epsilon ~a"  cmd))
  (define gen-hdlr (mk-generic-hdlr 'epsilon epsilon-model))
  (match cmd
    [(op-create t cfg)
     (when (eq? 'fail-on-create (hash-ref cfg 'i1))
       (raise-result-error 'create-success "Error creating epsilon " cfg))
     (generic-create t cfg)]
    [(op-delete _t cfg)

     ;; resource will delete on the second attempt, which is prepared by updating the i1 field of the
     ;; sim resource to 'reset

     (when (and (eq? 'fail-on-delete (hash-ref cfg 'i1))
                (eq? 'fail-on-delete (hash-ref (hash-ref sim-resources (hash-ref cfg 'id)) 'i1)))
       (generic-update (hash-set cfg 'i1 'reset) null)
       (raise-result-error 'delete-success "Error deleting epsilon " cfg))
     (generic-delete cfg)]
    [_ (gen-hdlr cmd) ]))

(define (gamma-hdlr cmd)

  ;; gamma resources are hard-dependents of any other resource; a gamma is created with an immutable
  ;; reference (i1) to any sim resource, and that target sim resource cannot be deleted/recreated
  ;; unless the gamma resource is destroyed first.

  (log-marv-debug (format "gamma ~a"  cmd))
  (define gen-hdlr (mk-generic-hdlr 'gamma gamma-model))
  (match cmd
    [(op-create type cfg) (create-with-gamma-ref type cfg)]
    [(op-delete _t cfg)
     (delete-gamma-ref cfg)
     (generic-delete cfg) ]
    [_ (gen-hdlr cmd)]
    ))

(define lookups
  (mkih
   alpha (mk-generic-hdlr 'alpha alpha-model)
   beta (mk-generic-hdlr 'beta beta-model)
   delta delta-hdlr
   epsilon epsilon-hdlr
   gamma gamma-hdlr
   ))

(define sim-resource-types (hash-keys lookups))

(define simdrv
  (mk-res-driver
   'simdriver
   (lambda (cmd)
     ((hash-ref lookups (op-cmd-type cmd)) cmd))))

(define (sim-driver id) (driver (res-driver-id simdrv) id (res-driver-exec simdrv) null))

;;
;; debug
;;

;; (define sd (sim-driver 'simd1))
;; sd
;; (define a1 (((alpha (mkih i1 '123)) sd) 'main.a1))
;; a1
;;
;; ((driver-func sd) (op-create 'alpha ((compose1 resolve-computations resource-cfg) a1)))
;; resources
;;

