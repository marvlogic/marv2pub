#lang racket

(require
  rackunit
  rackunit/text-ui
  racket/date
  marv/sim
  marv/state
  marv/sim-chk-utils
  marv/marvd
  marv/stx-utils
  marv/logger)

(displayln "")
(displayln "********************************************************************************")

(log-level-set 'debug)
(log-filter-set "(modules|compose|marvd|tasks|.*barrier.*|sim|test-.*|state).rkt")
;; (log-filter-set "(state|modules.rkt)")

(define (main-drivers)
  (:= marvd marv-driver)
  (:= sim sim-driver)
  #t)


(define (main #:cidr cidr #:env [envid "dev"])

  (:= marvd marv-driver)
  (:= sim sim-driver)

  ;; Manual way of defining a checkpoint set
  (:> vpc := sim :: alpha i1 cidr)
  (:> sn1 := sim :: gamma m1 "subnet1" i1 (rref vpc id))
  (:> sn2 := sim :: gamma m1 "subnet2" i1 (rref vpc id))
  ;; Note, only possible to pass attributes through a checkpoint - not full resources!
  (:> ck1 := marvd :: checkpoint vpc (rref vpc id) sn1 (rref sn1 id) sn2 (rref sn2 id))
  (:> inst1 := sim :: beta i1 (rref ck1 sn1) i2 envid)

  (>>: ck2 marvd
       (:> vpc := sim :: alpha i1 cidr)
       (:> bsn1 := sim :: gamma m1 "subnet1" i1 (rref vpc id))
       (<= vpc (rref vpc id) sn1 (rref bsn1 id)))

  (:> inst2 := sim :: beta i1 (rref ck2 sn1) i2 envid)

  #t

  )

(exit
  (run-tests
    (test-suite
      "Test suite 1"
      (test-case
        "Simple config"
        (reset-simulation)
        (check-sim-vs-reality)
        (mk-plan-m (lambda () (main #:cidr "10.0.0.0/24")))
        ;; (check-eq? (length (plan-keys)) 14)
        (apply-plan)
        (displayln (date->string (current-date) #t))
        ;; (exit 0)
        (check-sim-vs-reality)
        (mk-plan main-drivers)
        (apply-plan)
        (check-sim-vs-reality)
        (displayln (date->string (current-date) #t))
        )
      )
    )
  )
