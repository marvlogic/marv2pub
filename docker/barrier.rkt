#lang racket

(provide install-docker-barriers)

(require
  marv/gen-barriers
  marv/state
  marv/resources
  marv/barrier
  marv/logger
  )

(define (docker-job-success-barrier bcfg contn)
  (define k (barrier-cfg-id bcfg))
  (lambda()
    (define cfg (computation-hs (resource-cfg (state-ref k))))
    (if (eq? (hash-ref cfg 'exit-status) 0)
        (contn)
        (error 'docker "job failed exit status check, log file: ~a "
               (hash-ref cfg 'logfile)))))

(define (docker-job-barrier bcfg contn)
  (wait-for-state-barrier
   (lambda(cfg)(hash-ref cfg 'exit-status)) bcfg (docker-job-success-barrier bcfg contn)))

(define (barrier-match-post bcfg)
  (match bcfg
    [(barrier-cfg _ 'create (resource 'docker-job _ _ _)) docker-job-barrier]
    [_ #f]))

(define (install-docker-barriers)
  (barrier-set 'post barrier-match-post))
