#lang racket

(provide
 (struct-out task)
 run-tasks)

(require
  marv/logger)

(struct task (id func excn-handler))

(define default-delay
  (let ([ev (getenv "MARV_TQ_DELAY")])
    (if ev (string->number ev) 1)))

(define (run-tasks ts #:delay (delay default-delay))

  (define new-ts
    (for/fold ([tacc '()]) ([t ts])
      (define exn-handler (task-excn-handler t))
      (define fn (task-func t))
      (define id (task-id t))
      (define result
        (if (procedure? exn-handler)
            (with-handlers ([exn:fail? (lambda(e)(exn-handler id e))])
              (fn))
            (fn)))
      (log-marv-debug result)
      (cond
        [(procedure? result) (cons (struct-copy task t [func result]) tacc)]
        [else tacc]
        )))
  (log-marv-debug (~a "new tasks " new-ts))
  (unless (null? new-ts)
    (log-marv-debug (~a "Running new list in " delay "s"))
    (sleep delay)
    (run-tasks (reverse new-ts) #:delay delay)))

