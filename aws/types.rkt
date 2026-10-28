#lang racket

(provide
 define/hdlr
 types-lookup)

(require marv/aws/syntax)

(define types-lookup (make-hash))

(define-syntax (define/hdlr stx)
  (syntax-case stx ()
    ([_ ...] (define-lookup-hdlr #'types-lookup stx))))
