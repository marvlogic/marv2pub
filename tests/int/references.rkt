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

(log-level-set 'info)
(log-filter-set "(references|marvd|tasks|.*barrier.*|sim|test-.*|state).rkt")

(define (main-drivers)
  (:= marvd marv-driver)
  (:= sim sim-driver)
  #t)

(define (main-base)
  (:= marvd marv-driver)
  (:= sim sim-driver)

  (:> vpc := sim :: alpha i1 "10.0.0.0/24")
  ;; (::= vid marvd svar (rref vpc id))
  (:> sn1 := sim :: gamma m1 "subnet1" i1 (rref vpc id))

  (:> net1 := marvd :: checkpoint
      vpcid (rref vpc id)
      sn1 (rref sn1 id))

  (:> inst1.inst := sim :: beta i1 (rref net1 sn1))
  (:> inst := marvd :: checkpoint
      instid (rref inst1.inst id))

  (hash 'sim sim 'sn1 sn1))

(exit
 (run-tests
  (test-suite
   "Test suite 1"
   (test-case
    "Simple config"
    (reset-simulation)
    (check-sim-vs-reality)
    (mk-plan main-base)
    ;; (check-eq? (length (plan-keys)) 8)
    (apply-plan)
    (displayln (date->string (current-date) #t))
    (read)
    (check-sim-vs-reality)
    (mk-plan main-drivers)
    (apply-plan)
    (check-sim-vs-reality)
    (displayln (date->string (current-date) #t))
    )
   )
  )
 )
