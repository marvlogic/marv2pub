#lang racket

(provide
 model-diff-hs
 hash->model-attrs
 generic-diff
 (struct-out model-attrs)
 mk-blueprint-fn
 )

(require
  racket/hash
  marv/resources
  )

(struct model-attrs (all allowed mutable))

;; TODO: *** Unit tests around key components like this one, and its callers

(define (model-diff-hs model oldhs newhs)
  (define attr-perms (model-attrs-allowed model))
  (define (filter-attrs k _v) (hash-has-key? attr-perms k))
  (define ocfg (hash-filter oldhs filter-attrs))
  (define ncfg (hash-filter newhs filter-attrs))

  (define (df o n mode)
    (cond
      [(eq? 'undef n) #f]
      [(equal? o n) #f]
      [else (if (eq? 'immutable mode) (attr-replace n) (attr-update n))]))

  (for/fold ([hs (hash)])
            ([(attr mode) (in-hash attr-perms)])
    (define d (df (hash-ref ocfg attr 'undef) (hash-ref ncfg attr 'undef) mode))
    (if d (hash-set hs attr d) hs)))

(define (hash->model-attrs hs)
  (define allowed (hash-filter hs (lambda (_ v)(not (eq? v 'read-only)))))
  (define mutable (hash-filter hs (lambda (_ v)(eq? v 'mutable))))
  (model-attrs hs allowed mutable))

(define (generic-diff model oldcfg newcfg #:mdl->diff [mdl->diff values])
  (define diff (model-diff-hs model (mdl->diff oldcfg) (mdl->diff newcfg)))
  (op-diff-result diff (immutable-change? diff)))

(define (mk-blueprint-fn model)
  (lambda(val-fn)
    (for/hash ([(k _) (in-hash model)])
      (values k (lambda()(safe-hash-ref (val-fn) k))))))
