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

(sim-persist-state "module-sim2.simstate.dat")

;; (barrier-set 'post gen-barrier-match)
;; (barrier-set 'pre igw-barrier-match)

(define (main [purge? #f])
  (:= svard svar-driver)
  (:= sim sim-driver)

  (when (not purge?)
    (:> vpc := sim :: alpha i1 "11.0.0.0/24")
    (::= vid svard svar (rref vpc id))
    (:> sn1 := sim :: gamma m1 "subnet" i1 (vid))
    (:> inst1 := sim :: epsilon m1 "instance" m1 (rref sn1 id) i1 'fail-on-delete)
    (:> ip-assign := sim :: alpha m1 "ip-assignment" i1 (rref inst1 id))
    #t)
  #t)
