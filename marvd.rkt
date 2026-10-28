#lang racket

(provide
  marv-driver
  checkpoint
  >>:
  svar)

(require
  (for-syntax syntax/parse)
  marv/logger
  marv/state
  marv/resources)

(define (((svar v) drv) id)
  (mk-resource 'svar id drv v))

;; TODO Checkpoints cannot hold complete resources; only (rref'd) attributes of resources!

(define (((checkpoint ids) drv) id)
  (mk-resource 'checkpoint id drv ids))

; TODO: this only handles primitive vars, not structured data
(define (svar-hdlr cmd)
  (match cmd
    [(op-model _ _) (hash 'val 'immutable)]
    [(op-create _ cfg) cfg]
    [(op-diff _t _o newv)
     (if (procedure? newv)
         (op-diff-result (hash) #t)
         (op-diff-result (hash) #f))]
    [(op-cmd _ cfg) cfg]))

(define (checkpoint-hdlr cmd)
  (match cmd
    [(op-model _ _) (hash 'ids 'immutable)]
    [(op-create _ cfg) cfg]
    [(op-diff _t oldv newv)
     (if (equal? oldv newv)
         (op-diff-result (hash) #f)
         (op-diff-result (hash) #t))]
    [(op-cmd _ cfg) cfg]))

(define types-lookup
  (hash 'svar svar-hdlr
        'checkpoint checkpoint-hdlr))

(define lcdriver
  (mk-res-driver
    'marv
    (lambda (lc-cmd)
      (define hdlr (hash-ref types-lookup (op-cmd-type lc-cmd)
                             (lambda () (raise (~a "Type handler not found:" lc-cmd)))))
      (log-marv-debug (~a "lc-handler " hdlr))
      (define result (hdlr lc-cmd))
      (log-marv-debug (~a "result " result))
      (if (op-diff? lc-cmd)
          (if (op-diff-result? result) result (op-diff-result (hash) #f))
          result)
      )))

(define marv-driver (lambda (id) (driver (res-driver-id lcdriver) id (res-driver-exec lcdriver) null)))

(define-syntax (>>: stx)
  (syntax-parse stx
    #:literals (<=)
    [(_ lhs m stmt ... (<= boxout ...))
     (with-syntax
       ([file (syntax-source stx)]
        [line (syntax-line stx)])
       #'(:= lhs
             ;; Lambda is only to get the effect of allowing 'define' in this context
             ((lambda ()
                stmt ...
                (:: m checkpoint (<= boxout ...))
                )))
       )]
    ))

