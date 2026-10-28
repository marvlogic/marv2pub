#lang racket

(struct computation2 (op one two) #:transparent)
(define (apply2 fn one two)
  (match (list one two)
    [(list x y)
     #:when (or (computation2? one)(computation2? two))
     (computation2 fn x y)]

    [(list x y)
     #:when (and (number? one) (number? two))
     (fn one two)]

    [else #f]))

; experiments with apply and computations
(apply + (list 1 2 (apply * (list 3 3))))

(apply2 + 1 2)
(apply2 + 1 "s")
(apply2 + 99 (computation2 void 1 #f))


(struct computation (op args) #:transparent)
(define (apply3 op . rst)
  (cond
    [(memf computation? rst) (computation op rst)]
    [else (apply op rst)]))

(apply3 + 1 2 (computation hash-ref (list (hash 'attr 123) 'attr)))
(apply3 + 1 2 (computation apply (lambda()'val)))

(define (resolve-computations lst)
  (for/list ([c lst])
    (if (computation? c)
        (apply (computation-op c) (computation-args c))
        c)))

(resolve-computations (list 1 2 (computation hash-ref (list (hash 'attr 123) 'attr))))
(resolve-computations (list 1 2 (computation (lambda()'val) '())))
(resolve-computations (list 1 2 (computation (lambda()(computation void '())) '())))

(displayln "COMPUTATIONS")

;; A Reader is just a function: env -> value
(define (reader-return x)
  (lambda (env) x))

(define (reader-bind m f)
  (lambda (env)
    (define a (m env))
    ((f a) env)))

;; Helper to get the current environment
(define reader-ask
  (lambda (env) env))

;; Example: a computation that adds env to a number
(define (add-env n)
  (lambda (env) (+ n env)))

;; Use bind to compose computations
(define c3
  (reader-bind
   (add-env 10)
   (lambda (result)
     (reader-bind
      reader-ask
      (lambda (env)
        (reader-return (* result env))))))
  )

;; Run the Reader
(displayln (c3 3))  ; => (10 + 3) * 3 = 39

(define (create-vpc x) (hash 'id x))
(define (create-subnet vpc_id) (hash 'vpc_id vpc_id))
