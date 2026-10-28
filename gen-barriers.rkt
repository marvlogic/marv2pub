#lang racket

(provide
 random-counter-barrier
 wait-for-state-barrier)

(require
  marv/state
  marv/resources
  marv/barrier
  marv/logger)

(define (random-counter-barrier bcfg contn)
  (define cnt (random 5))
  (define k (barrier-cfg-id bcfg))
  (define (chk)
    (cond
      [(> cnt 0)
       (log-marv-debug (~a "counter " cnt))
       (set! cnt (sub1 cnt))
       chk]
      [else
       (log-marv-debug (~a "barrier for " k " passed, proceeding "))
       (contn)]))
  (chk))

(define (wait-for-state-barrier chkfn bcfg contn)
  (define k (barrier-cfg-id bcfg))
  (define (chk)
    (refresh-resource-state k)
    (define st (state-ref k))
    (define cfg (computation-hs (resource-cfg st)))
    (if (chkfn cfg) (contn) chk))
  (chk))

