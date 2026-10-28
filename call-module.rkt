#lang racket

;; This is early attempt to formalise modules
(require marv/sim
         marv/state
         marv/stx-utils
         marv/marvd
         marv/logger
         marv/resources)


(define (net-defs #:sim sim #:cidr cidr)
  (:> vpc := sim :: alpha i1 cidr)
  (:> sn1 := sim :: gamma m1 "subnet1" i1 (rref vpc id))
  (:> sn2 := sim :: gamma m1 "subnet2" i1 (rref vpc id))
  (values (lambda () (rref sn1 id)) (lambda () (rref sn2 id)))
  )
;; Simple definition of resources in a function - lacking a nested module
;; context though...
;; (define-values (sn1 sn2) (net-defs #:sim sim #:cidr cidr))

(define (net-module #:marvd marvd
                    #:sim sim
                    #:cidr cidr)
  (:> vpc := sim :: alpha i1 cidr)
  (:> sn1 := sim :: gamma m1 "subnet1" i1 (rref vpc id))
  (:> sn2 := sim :: gamma m1 "subnet2" i1 (rref vpc id))
  (:: marvd checkpoint
      (<=
        sn1 (rref sn1 id)
        sn2 (rref sn2 id))))

(define (instance-module #:marvd marvd #:sim sim #:snid snid)
  (:> inst := sim :: delta i1 snid)
  (:: marvd checkpoint (<= id (rref inst id))))

(define (state-find-module-params mod-id params)
  (hash 'cidr "10.0.0.9/16"))


;; (:= net1 (net-module #:marvd marvd #:sim sim #:cidr cidr))
;; (:= net1 (call-module "net1" net-module (hash 'marvd marvd 'sim sim 'cidr cidr)))
;; (:= net2 (call-module "net2" net-module (hash 'marvd marvd 'sim sim 'cidr cidr)))
;; (:= inst1 (call-module "inst1" instance-module (hash 'marvd marvd 'sim sim 'snid (rref net1 sn1))))
(require marv/resources)
(define (call-module mod-id mod-func params-hs)

  ;; Should add Parent dependency??

  (define mod-params (state-find-module-params mod-id params-hs))
  (define (replace-if-seed k)
    (if (eq? (hash-ref params-hs k 'seed) 'seed)
        (hash-ref mod-params k)
        (hash-ref params-hs k)))

  (define-values (required accepted) (procedure-keywords mod-func))
  (log-marv-debug (~a "Keywords: " required accepted))
  (define keyword->symbol (compose1 string->symbol keyword->string))
  (define assembled-params
    (for/hash ([kw (map keyword->symbol accepted)])
      (values kw (replace-if-seed kw))))
  (log-marv-debug assembled-params)

  (define kvs (hash->list assembled-params #t))
  (define symbol->keyword (compose1 string->keyword symbol->string car))
  (keyword-apply mod-func (map symbol->keyword kvs) (map cdr kvs) '())
  (lambda (x) (displayln x) (resource 'module mod-id 'driver-here assembled-params)))

