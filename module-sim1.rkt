#lang racket

(provide main)

(require
  marv/sim
  marv/sim-chk-utils
  marv/state
  marv/barrier
  marv/sim-chk-utils
  marv/svar
  marv/stx-utils
  )

(sim-persist-state "module-sim1.simstate.dat")

(barrier-set 'post gen-barrier-match)
(barrier-set 'pre igw-barrier-match)

(displayln  "******************")
(displayln  "* IGW Simulation *")
(displayln  "******************")

(define (main [purge? #f])
  (:= svard svar-driver)
  (:= sim sim-driver)

  (when (not purge?)
    (:> vpc := sim :: alpha i1 "11.0.0.0/24")
    (::= vid svard svar (rref vpc id))
    (:> sn1 := sim :: gamma m1 "subnet" i1 (vid))
    (:> inst1 := sim :: delta m1 "instance" i1 (rref sn1 id))
    (:> inst2 := sim :: delta m1 "instance" i1 (rref sn1 id))
    (:> igwatt := sim :: beta m1 "igw1" vpc-id (vid))
    #t)
  #t)
