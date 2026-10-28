#lang racket

(require
  rackunit
  rackunit/text-ui
  marv/resolver
  marv/sim
  marv/state
  marv/sim-chk-utils
  marv/svar
  marv/stx-utils
  marv/logger)

(displayln "")
(displayln "********************************************************************************")

(log-level-set 'info)
(log-filter-set "(tasks|.*barrier.*|sim|test-.*|state).rkt")

(define (main-drivers)
  (:= svard svar-driver)
  (:= sim sim-driver)
  (values sim svard))

(define (main-base sim svard inst-mode [purge? #f])
  (when (not purge?)
    (:> vpc := sim :: alpha i1 "10.0.0.0/24")
    (::= vid svard svar (rref vpc id))
    (:> sn1 := sim :: gamma m1 "subnet" i1 (vid))
    (:> inst1 := sim :: epsilon m1 (rref sn1 id) i1 inst-mode)
    (:> ip-assoc := sim :: alpha m1 (rref inst1 id) )
    #t))

(exit
 (run-tests
  (test-suite
   "Exception and recovery"
   (test-case
    "Pre-existing drivers in state"
    (reset-simulation)
    (mk-plan (lambda() (define-values (_s _ss) (main-drivers)) #t))
    (check-eq? (length (plan-keys)) 2)
    (define report (apply-plan))

    (mk-plan (lambda()
               (define-values (sim svard) (main-drivers))
               (main-base sim svard 'fail-on-create)))

    (check-eq? (length (plan-keys)) 5)
    (check-plan "main.vpc" plan-op-create?)
    (check-plan "main.vid" plan-op-create?)
    (check-plan "main.sn1" plan-op-create?)
    (check-plan "main.inst1" plan-op-create?)
    (check-plan "main.ip-assoc" plan-op-create?)

    (set! report (apply-plan-expect-failure))
    (check-equal? (hash-ref report "main.vpc") 'done)
    (check-equal? (hash-ref report "main.vid") 'done)
    (check-equal? (hash-ref report "main.sn1") 'done)
    (check-equal? (hash-ref report "main.inst1") 'failed)
    (check-equal? (hash-ref report "main.ip-assoc") 'failed)

    (check-equal? (resolver-parse-cmd "set main.inst1 i1 safe" report) report)
    )

   ;; (require rackunit/gui) (test/gui #:wait? #t (test-suite "Suite1"
   (test-case
    "Failure handling"
    (reset-simulation)
    (mk-plan
     (lambda()
       (define-values (sim svard) (main-drivers))
       (main-base sim svard 'fail-on-create)))
    (check-eq? (length (plan-keys)) 7)
    (check-plan "main.vpc" plan-op-create?)
    (check-plan "main.vid" plan-op-create?)
    (check-plan "main.sn1" plan-op-create?)
    (check-plan "main.inst1" plan-op-create?)
    (check-plan "main.ip-assoc" plan-op-create?)
    (check-dependents "main.vpc" '("main.vid"))
    (check-dependents "main.vpc" '("main.vid"))
    (check-dependents "main.vid" '("main.sn1"))
    (check-dependents "main.sn1" '("main.inst1"))
    (check-dependents "main.inst1" '("main.ip-assoc"))

    (define report (apply-plan-expect-failure))
    (define (resolver-cmd cmd) (set! report (resolver-parse-cmd cmd report)))

    (check-equal? (hash-ref report "main.inst1") 'failed)
    (check-equal? (hash-ref report "main.ip-assoc") 'failed)

    (check-sim-vs-reality)

    (check-equal? (resolver-parse-cmd "set main.inst1 i1 safe" report) report)
    (resolver-cmd "apply main.inst1")
    (check-equal? (hash-ref report "main.inst1") 'done)
    (resolver-cmd "apply main.ip-assoc")
    (check-equal? (hash-ref report "main.ip-assoc") 'done)

    (check-sim-vs-reality)
    (finish-application)

    (mk-plan
     (lambda()
       (define-values (sim svard) (main-drivers))
       (main-base sim svard 'safe)))

    (check-eq? (length (plan-keys)) 0)

    )

   (test-case
    "Failure handling - apply *"
    (reset-simulation)
    (mk-plan
     (lambda()
       (define-values (sim svard) (main-drivers))
       (main-base sim svard 'fail-on-create)))
    (check-eq? (length (plan-keys)) 7)
    (define report (apply-plan-expect-failure))
    (define (resolver-cmd cmd) (set! report (resolver-parse-cmd cmd report)))
    (check-equal? (hash-ref report "main.inst1") 'failed)
    (check-equal? (hash-ref report "main.ip-assoc") 'failed)
    (check-sim-vs-reality)
    (check-equal? (resolver-parse-cmd "set main.inst1 i1 safe" report) report)
    (resolver-cmd "apply *")
    (check-equal? (hash-ref report "main.inst1") 'done)
    (check-equal? (hash-ref report "main.ip-assoc") 'done)
    (check-sim-vs-reality)
    (finish-application)

    (mk-plan
     (lambda()
       (define-values (sim svard) (main-drivers))
       (main-base sim svard 'safe)))
    (plan-keys)
    (check-eq? (length (plan-keys)) 0)
    )

   (test-case
    "Failure handling  - failed delete"
    (reset-simulation)
    (mk-plan
     (lambda()
       (define-values (sim svard) (main-drivers))
       (main-base sim svard 'fail-on-delete)))
    (check-eq? (length (plan-keys)) 7)
    (define report (apply-plan))
    (define fail-ids (hash-keys (report-filter report 'failed)))
    (define (resolver-cmd cmd) (set! report (resolver-parse-cmd cmd report)))
    (check-eq? fail-ids '())
    (check-sim-vs-reality)
    (mk-plan
     (lambda()
       (define-values (sim svard) (main-drivers))
       (main-base sim svard 'fail-on-delete #t)))
    (check-eq? (length (plan-keys)) 5)
    (set! report (apply-plan-expect-failure))
    (set! fail-ids (hash-keys (report-filter report 'failed)))
    (check-eq? (length fail-ids) 4)
    (resolver-cmd "apply main.inst1")
    (set! fail-ids (hash-keys (report-filter report 'failed)))
    (displayln fail-ids)
    (check-eq? (length fail-ids) 3)
    (resolver-cmd "apply *")
    (set! fail-ids (hash-keys (report-filter report 'failed)))
    (check-eq? fail-ids '())
    (finish-application)
    (check-sim-vs-reality)

    )
   )
  )
 )
