#lang racket

(require
  rackunit
  rackunit/text-ui
  marv/sim
  marv/state
  marv/barrier
  marv/sim-chk-utils
  marv/svar
  marv/stx-utils
  marv/resources
  marv/gen-barriers
  marv/logger)

(displayln "")
(displayln "********************************************************************************")

(log-level-set 'debug)
(log-filter-set "(tasks|.*barrier.*|sim|test-.*|state).rkt")

(define (main-drivers)
  (:= svard svar-driver)
  (:= sim sim-driver)
  #t)

(define (main-base)
  (:= svard svar-driver)
  (:= sim sim-driver)

  (:> vpc := sim :: alpha i1 "10.0.0.0/24")
  (::= vid svard svar (rref vpc id))
  (:> sn1 := sim :: beta m1 "subnet1" i1 (vid))
  (:> sn2 := sim :: beta m1 "subnet2" i1 'other-vpc)

  (hash 'sim sim 'sn1 sn1 'sn2 sn2))

(define (main-loops)
  (define main (main-base))
  (define sim (hash-ref main 'sim))
  (define sn1 (hash-ref main 'sn1))
  (define sn2 (hash-ref main 'sn2))
  (:= instance for/res ([instance '(1 2 3)])
      (:> n1 := sim :: delta m1 "instance" i1 (rref sn1 id))
      (:> n2 := sim :: delta m1 "instance" i1 (rref sn2 id))
      (list n1 n2))
  #t)

(exit
 (run-tests
  (test-suite
   "Test suite 1"
   (test-case
    "Loops and loops"
    (reset-simulation)
    (check-sim-vs-reality)
    (mk-plan main-base)
    (check-eq? (length (plan-keys)) 6)
    (apply-plan)
    (check-sim-vs-reality)
    (mk-plan main-loops)
    (check-eq? (length (plan-keys)) 6)
    (apply-plan)
    (check-sim-vs-reality)
    )
   )
  )
 )
