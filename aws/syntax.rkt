#lang racket


;; TODO: replace with stx-utils module

(provide (for-syntax define-lookup-hdlr))

(require (for-syntax syntax/parse racket/syntax))

(define-for-syntax (define-lookup-hdlr tlu stx)
  (syntax-parse stx
    [(_ (id args ...) body ...)
     (with-syntax
         ([hdlr-id (format-id stx "~a-hdlr" #'id)]
          [tlu tlu]
          [hash-id (format-symbol "aws-~a" #'id)])
       #'(begin
           (define (hdlr-id args ...) body ...)
           (hash-set! tlu 'hash-id hdlr-id)))]))

;; e.g.
;; (define type-lookups (make-hash))
;; (define-syntax (define/lup stx)
;;   (syntax-case stx ()
;;     ([_ any ...] (define/hdlr #'type-lookups stx))))
