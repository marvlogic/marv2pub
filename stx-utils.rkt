#lang racket

(provide
  mkih
  rref
  (for-syntax define-lookup-hdlr)
  )

(require (for-syntax syntax/parse racket/syntax))

(define-syntax-rule (rref r attr) (hash-ref (r) 'attr))

(define-syntax (mkih stx)
  (syntax-parse stx
    [(_ (~seq a b) ...) #'(hash (~@ 'a b) ...)]))

(define-for-syntax (define-lookup-hdlr tlu stx)
  (syntax-parse stx
    [(_ (id args ...) body ...)
     (with-syntax
       ([hdlr-id (format-id stx "~a-hdlr" #'id)]
        [tlu tlu]
        [hash-id (format-symbol "~a" #'id)])
       #'(begin
           (define (hdlr-id args ...) body ...)
           (hash-set! tlu 'hash-id hdlr-id)))]))

