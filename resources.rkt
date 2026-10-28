#lang racket

(provide (all-defined-out))

(require marv/utils)

(struct resource (type id drvr cfg) #:prefab)

(struct computation (hs) #:transparent)

; TODO: do we need cfg on drivers? Just bind config under a lambda?
(struct driver (type id func cfg) #:prefab)

(define (safe-hash-ref hs k) (if (undef? hs) hs (hash-ref hs k 'undef)))
(define (safe-car ls) (if (undef? ls) 'undef (car ls)))

(define (resolve-computations cmptn)

  ;; given a computation structure, resolve all computations (i.e. call any functions) contained in
  ;; it, returning the underlying data. If not resolved then the function is left in place (see
  ;; resolve-computations/filter below)

  (define (resolve v) (if (procedure? v) (v) v))

  (define (handle-list l)
    ;; FIXME: why is reverse necessary?
    (reverse
     (for/list ([v (in-list l)])
       (cond
         [(hash? v) (handle-hash v)]
         [(list? v) (handle-list v)]
         [else (resolve v)])) ))

  (define (handle-hash h)
    (for/hash ([(k v) (in-hash h)])
      (values
       k
       (cond
         [(hash? v) (handle-hash v)]
         [(list? v) (handle-list v)]
         [else (resolve v)]))))

  (define val (computation-hs cmptn))
  (cond
    [(hash? val) (handle-hash val)]
    [else (resolve val)]
    ))

(define (resolve-computations/filter cmptn)

  ;; Tries to resolve all the computations via resolve-computations. Any remaining computations after
  ;; this are filtered out of the result.

  (define (filter-cmptns _ v)
    ;; Filter out procedures, empty lists or empty hashes
    (not (or (procedure? v)
             (and (list? v) (empty? v))
             (and (hash? v) (hash-empty? v)))))

  (define cfg (resolve-computations cmptn))
  (cond
    [(hash? cfg) (hash-filternc cfg filter-cmptns)]
    [else cfg]))

(define (undef? x)
  (eq? 'undef
       (if (pair? x)
           (car x)
           x)))

(define (when-undef? x default) (if (undef? x) default x))

(define mk-res-driver cons)
(define res-driver-id car)
(define res-driver-exec cdr)

(define (mk-resource type id drvr cfg)
  (resource type id (driver-id drvr) (computation cfg)))

(define (resource-update-cfg r update-fn)
  (struct-copy resource r [cfg (computation (update-fn (computation-hs (resource-cfg r))))]))

(struct attr-op (val) #:prefab)
(struct attr-replace attr-op ())
(struct attr-update attr-op ())

(struct op-cmd (type cfg) #:prefab)
;;(struct op-none op-cmd () #:prefab)
(struct op-create op-cmd () #:prefab)
(struct op-read op-cmd () #:prefab)
(struct op-update op-cmd (diff) #:prefab)
(struct op-delete  op-cmd () #:prefab)
(struct op-diff op-cmd (new-cfg) #:prefab)
(struct op-model op-cmd () #:prefab)

(struct op-diff-result (hs immutable-change?) #:prefab)

(define (immutable-change? diff)
  (define-values (update? replace?)
    (for/fold ([update #f][replace #f])
              ([v (hash-values diff)])
      (cond
        [(attr-replace? v) (values update #t)]
        [(attr-update? v) (values  #t replace)]
        [else (values update replace)])))

  (if replace? #t #f))
