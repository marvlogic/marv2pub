#lang racket


(provide
 resolver-interact
 resolver-parse-cmd
 )

(require
  marv/state
  marv/resources
  )

(define (resolver-interact plan-report)
  (define report plan-report)
  (list-plan-items (report-filter report 'failed))
  (define (repl cmd contn)
    (when (equal? "exit" cmd) (contn))
    (set! report (resolver-parse-cmd cmd report))
    (printf "MARV>> ")
    (repl (read-line) contn))

  (printf "MARV>> ")
  (call/cc (lambda(k) (repl (read-line) k)))
  (displayln "BYE"))

(define (resolver-parse-cmd cmd plan-reports)
  (define failed-current (report-filter plan-reports 'failed))
  (define new-reports
    (match cmd
      [(regexp #px"list$") (list-plan-items failed-current)]
      [(regexp #px"list-state$") (list-state-ids)]
      [(regexp #px"apply ([\\w\\.-]+|\\*)"
               (list _ maybe-ids))
       (if (equal? maybe-ids "*")
           (plan-apply #:prior-report plan-reports #:ids (hash-keys failed-current))
           (plan-apply #:prior-report plan-reports #:ids (list maybe-ids))) ]
      [(regexp #px"state ([\\w\\.-]+)" (list _ id)) (show-state-item-details id)]
      [(regexp #px"show ([\\w\\.-]+)" (list _ id)) (show-plan-item-details id)]
      [(regexp #px"set ([\\w\\.-]+) ([\\w\\.-]+) ([\\w\\.-]+)" (list _ id key val))
       (define hkey (string->symbol key))
       (define hval (string->symbol val))
       (plan-update id (lambda(cfg)(hash-set cfg hkey hval)))
       (show-plan-item-details id)
       ]
      [_ (printf "ERROR: unknown command ~a\n" cmd)]))
  (if (hash? new-reports) new-reports plan-reports))

(define (list-plan-items reports)
  (define (status-of p)
    (cond
      [(member p '()) "FAILED"]
      [else "APPLIED"]))
  (define (print-item i p)
    (printf "~a: ~a\n" i p))

  (hash-map reports print-item))

(define (show-plan-item-details p)
  (define item-repr
    (match (plan-ref p)
      [(plan-op-create r) (computation-hs (resource-cfg r))]
      [(plan-op-replace _ r) (computation-hs (resource-cfg r))]
      [(plan-op-update _ r) (computation-hs (resource-cfg r))]
      [_ "UNKNOWN"]))
  (pretty-display item-repr))

(define (list-state-ids) (pretty-display (state-keys)))
(define (show-state-item-details id) (pretty-display (state-ref id)))

(define (update-plan-item p key val)
  (define item-repr
    (match (plan-ref p)
      [(plan-op-create r) (computation-hs (resource-cfg r))]
      [_ "Unsupported"]))
  (plan-update p (hash-set item-repr key val))
  )

