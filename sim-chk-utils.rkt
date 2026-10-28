#lang racket

(provide
 st-wrapper
 mk-plan
 mk-plan-m
 apply-plan
 apply-plan-expect-failure
 finish-application
 reset-simulation
 check-plan
 check-id-changed
 check-dependents
 beta-instances-check
 beta-invariant-check
 delta-barrier
 delta-delete-barrier
 gen-barrier-match
 igw-barrier-match
 check-sim-vs-reality)

(require
  rackunit
  racket/hash
  marv/gen-barriers
  marv/barrier
  marv/resources
  marv/sim
  marv/logger
  marv/graph
  marv/state)

(require/expose marv/state (state-graph))

(define (st-wrapper thk)
  (with-handlers
      ([exn:fail?
        (lambda(e)
          (displayln (~a "Error during wrapper:" e ))
          (define cms (exn-continuation-marks e))
          (define context (continuation-mark-set->context cms))
          (for-each (lambda (frame) (printf "~a\n" frame)) context)
          ;; (sim-save-state sim-state-file)
          ;; (state-save "state-int.dat")
          (exit 1))])
    (thk)))

(define statefile "simstate.dat")

(define (reset-simulation)
  (sim-init)
  (when (file-exists? statefile) (delete-file statefile))
  (state-init))

(define (mk-plan-m . modules)
  (define (main-module)
    (map (lambda(f)
           (displayln (format "running module: ~a" f))
           (apply f '())
           (displayln "Finished")) modules))
  (st-wrapper main-module)
  (displayln "PLAN")
  (plan-display))

(define (mk-plan . modules)
  (state-load statefile)
  (plan-init)
  (define (main-module)
    (map (lambda(f)
           (displayln (format "running module: ~a" f))
           (apply f '())
           (displayln "Finished")) modules))
  (st-wrapper main-module)
  (displayln "PLAN")
  (plan-display))

(define (apply-plan [dump-state? #f])
  (displayln "APPLYING")
  (define result (st-wrapper plan-apply))
  (finish-application)
  (state-save statefile)
  (when dump-state? (state-dump))
  result)

(define (apply-plan-expect-failure)
  (displayln "APPLYING, EXPECTING FAILURES")
  (define report (st-wrapper plan-apply))
  (log-marv-info "FINAL STATE")
  (check-not-eq? report (hash))
  ;; (state-save statefile)
  ;; (state-dump)
  report)

(define (finish-application)
  (log-marv-info "FINAL STATE")
  (state-save statefile))


(define-simple-check (check-plan id expected-op?)
  (expected-op? (plan-ref id)))

(define-simple-check (check-id-changed id old-ids new-ids)
  (not (eq? (hash-ref old-ids id) (hash-ref new-ids id))))

(define (check-state-invariants)
  (define state (state-repr))
  (define state-resource-keys (map resource-id (filter resource? (hash-values state))))
  #;(check-equal? (length state-stack-keys)
                  (length state-resource-keys)
                  (~a "stack invariant violation, stack has: " state-stack-keys ", resources has:" state-resource-keys))
  #;(check-true (set=? state-resource-keys state-stack-keys))
  #t
  )

(define (check-sim-vs-reality)
  (define (is-sim-resource? r) (memq (resource-type r) sim-resource-types))
  (define st-sim-resources (filter is-sim-resource? (state-resources)))

  (check-state-invariants)
  (define (mk-resource-id r) (~a (resource-id r) "/" (hash-ref (resource-cfg r) 'id)))
  (check-eq?
   (length st-sim-resources)
   (hash-count (hash-filter sim-resources (lambda(_ v) (not (hash-has-key? v 'deleted)))))
   (format "** State sim-resources: ~a. ** Sim resources ~a"
           (map mk-resource-id st-sim-resources)
           sim-resources))
  (for/hash ([st-res st-sim-resources])
    (log-marv-debug (~a "checking reality for: " st-res))
    (define stid (hash-ref (resource-cfg st-res) 'id))
    (check-true (hash-has-key? sim-resources stid))
    (check-equal? (hash-ref sim-resources stid) (resource-cfg st-res))
    (values (resource-id st-res) stid)))

(define-simple-check (check-dependents id deps)
  (define ds (graph-get-reverse-neighbours state-graph id))
  (check-equal? ds deps))


(define (beta-invariant-check k contn)
  (if (> instances-on-vpc 0)
      (raise 'beta-invariant-error)
      (contn)))

(define (beta-instances-check k contn)
  (define (chk)
    (if (= instances-on-vpc 0)
        (contn)
        chk))
  chk)

(define (igw-barrier-match bcfg)
  (define k (barrier-cfg-id bcfg))
  (define planop (plan-ref k))
  (match planop
    [(plan-op-delete (resource 'beta _ _ _)) beta-instances-check]
    [_ #f]))

(define instances-on-vpc 0)

(define (delta-barrier bcfg contn)
  (wait-for-state-barrier
   (lambda(c)
     (cond
       [(eq? 3 (hash-ref c 'ro1))
        (set! instances-on-vpc (add1 instances-on-vpc))
        #t]
       [else #f]))
   bcfg contn))

(define (delta-delete-barrier bcfg contn)
  (wait-for-state-barrier
   (lambda(c)
     (cond
       [(hash-has-key? c 'deleted)
        (set! instances-on-vpc (sub1 instances-on-vpc))
        #t]
       [else #f]))
   bcfg contn))

(define (gen-barrier-match bcfg)
  (define k (barrier-cfg-id bcfg))
  (define planop (plan-ref k))
  (match planop
    [(plan-op-create (resource 'alpha _ _ _)) random-counter-barrier]
    [(plan-op-delete (resource 'alpha _ _ _)) random-counter-barrier]
    [(plan-op-create (resource 'delta _ _ _)) delta-barrier]
    [(plan-op-delete (resource 'delta _ _ _)) delta-delete-barrier]
    [_ #f]))

