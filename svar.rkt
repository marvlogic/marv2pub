#lang racket

(provide svar svar-driver)

(require marv/resources marv/logger)

(define (((svar v )drv) id)(mk-resource 'svar id drv v))

; TODO: this only handles primitive vars, not structured data
(define (svar-hdlr cmd)
  (log-marv-warn  "SVAR is deprecated")
  (match cmd
    [(op-create _ cfg) cfg]
    [(op-diff _t oldv newv)
     (if (procedure? newv)
         (op-diff-result (hash) #t)
         (op-diff-result (hash) #f))]
    [(op-cmd _ cfg) cfg]))

(define svar-driver (lambda(id)(driver 'svar id  svar-hdlr null)))
