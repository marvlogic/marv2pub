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

(log-level-set 'debug)
(log-filter-set "(compose|marvd|tasks|.*barrier.*|sim|test-.*|state).rkt")

(define (main-drivers)
  (:= marvd marv-driver)
  (:= sim sim-driver)
  #t)

;; This needs to be a first-class item in marv - something that can have its own state and causes a
;; 'boundary' within the statefile. E.g. (def/mod ....)

;; parameters should be passed another way (named? or hash? or resource-func? )

(define (net-module marvd sim cidr)

  (:> vpc := sim :: alpha i1 cidr)
  (::= vid marvd svar (rref vpc id))
  (:> sn1 := sim :: gamma m1 "subnet1" i1 (vid))
  (:> sn2 := sim :: gamma m1 "subnet2" i1 (vid))

  ;; how to return things from a module? outputs?
  (:: marvd checkpoint
      (<=
       sn1 (rref sn1 id)
       sn2 (rref sn2 id)))
  )

(define (main-base)
  (:= marvd marv-driver)
  (:= sim sim-driver)

  (:= network (net-module marvd sim "10.0.0.2/16"))

  (:> inst := sim :: beta i1 (rref network sn1))

  (hash)

  )

(exit
 (run-tests
  (test-suite
   "Composing state files"
   (test-case
    "Simple config"
    (reset-simulation)
    (check-sim-vs-reality)
    (mk-plan main-base)
    (check-eq? (length (plan-keys)) 8)
    (apply-plan)
    (exit 0)
    (check-sim-vs-reality)
    (mk-plan main-drivers)
    (apply-plan)
    (check-sim-vs-reality)
    )
   )
  )
 )
