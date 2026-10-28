#lang racket

(provide
 barrier-set
 barrier-remove
 barrier-clear-all
 barrier-find-first
 (struct-out barrier-cfg)
 )

(require
  marv/logger
  )

(struct barrier-cfg (id op res))

(define barriers (make-parameter (hash)))

(define (barrier-set phase b)
  (define current-barriers (hash-ref (barriers) phase '()))
  (define new-barriers (cons b current-barriers))
  (barriers (hash-set (barriers) phase new-barriers)))

(define (barrier-remove b) void)

(define (barrier-clear-all) (barriers (hash)))

;; TODO: we need a different abstraction to cope with the delete vs replace issue i.e. don't just pass
;; 'k' along to the barrier checks, it needs more nuance. This is sort of fixed in state.rkt using
;; 'plan-set-delete-over-replace, but it's not ideal.

(define (barrier-find-first phase bcfg contn)
  (define foundb? (for/or ([b (hash-ref (barriers) phase '())]) (b bcfg)))
  (log-marv-info (~a "checking barriers " phase " found: " foundb?))
  (if foundb? (foundb? bcfg contn) (contn)))

