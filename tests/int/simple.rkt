#lang racket

(require
  rackunit
  rackunit/text-ui
  marv/sim
  marv/state
  marv/sim-chk-utils
  marv/marvd
  marv/stx-utils
  marv/logger)

(displayln "")
(displayln "********************************************************************************")

(log-level-set 'info)
(log-filter-set "(simple|marvd|tasks|.*barrier.*|sim|test-.*|state).rkt")
(log-filter-set "(simple|tasks|state).rkt")

(define (main-drivers)
  (:= marvd marv-driver)
  (:= sim sim-driver)
  #t)

(define (main-base)
  (:= marvd marv-driver)
  (:= sim sim-driver)

  (:> vpc := sim :: alpha i1 "10.0.0.0/24")
  (::= vid marvd svar (rref vpc id))
  (:> sn1 := sim :: gamma m1 "subnet1" i1 (vid))
  (:> sn2 := sim :: gamma m1 "subnet2" i1 (vid))
  (:> chkpt := marvd :: checkpoint
      sn1 (rref sn1 id)
      sn2 (rref sn2 id))
  (:> inst := sim :: beta i1 (rref chkpt sn1))

  (hash 'sim sim 'sn1 sn1 'sn2 sn2))

(exit
 (run-tests
  (test-suite
   "Test suite 1"
   (test-case
    "Simple config"
    (reset-simulation)
    (check-sim-vs-reality)
    (mk-plan main-base)
    (check-eq? (length (plan-keys)) 8)
    (apply-plan)
    (check-sim-vs-reality)
    (mk-plan main-drivers)
    (apply-plan)
    (check-sim-vs-reality)
    )
   )
  )
 )
