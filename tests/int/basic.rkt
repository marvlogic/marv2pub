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
  (:> sn1 := sim :: gamma m1 "subnet" i1 (vid))
  (:> sn2 := sim :: beta m1 "subnet2" i1 'other-vpc)

  #t)

(define (main-phase1)
  (:= svard svar-driver)
  (:= sim sim-driver)

  (:> vpc := sim :: alpha i1 "11.0.0.0/24")
  (::= vid svard svar (rref vpc id))
  (:> sn1 := sim :: gamma m1 "subnet" i1 (vid))
  (:> sn2 := sim :: beta m1 "subnet2->3" i1 'other-vpc)
  (:> inst1 := sim :: delta m1 "instance" i1 (rref sn1 id))

  ;; (:> ep1 := sim :: epsilon m1 "instance" i1 (rref sn1 id))
  #t)

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
  (match bcfg
    [(barrier-cfg _ 'create (resource 'alpha _ _ _)) random-counter-barrier]
    [(barrier-cfg _ 'delete (resource 'alpha _ _ _)) random-counter-barrier]
    [(barrier-cfg _ 'create (resource 'delta _ _ _)) delta-barrier]
    [(barrier-cfg _ 'delete (resource 'delta _ _ _)) delta-delete-barrier]
    [_ #f]))

(barrier-set 'post gen-barrier-match)

(exit
 (run-tests
  (test-suite
   "Test suite 1"


   (test-case
    "Stack up and down"
    (reset-simulation)
    (mk-plan main-phase1)
    (check-eq? (length (plan-keys)) 7)
    (check-plan "vpc" plan-op-create?)
    (check-plan "vid" plan-op-create?)
    (check-plan "sn1" plan-op-create?)
    (check-plan "sn2" plan-op-create?)
    (check-plan "inst1" plan-op-create?)
    (apply-plan)
    (displayln "**** CHECKPOINT 5")
    (check-sim-vs-reality)
    (mk-plan main-drivers)
    (check-eq? (length (plan-keys)) 5)
    (check-plan "vpc" plan-op-delete?)
    (check-plan "vid" plan-op-delete?)
    (check-plan "sn1" plan-op-delete?)
    (check-plan "sn2" plan-op-delete?)
    (check-plan "inst1" plan-op-delete?)
    (apply-plan)
    (displayln "**** CHECKPOINT 6")
    (check-sim-vs-reality)
    )

   (test-case
    "Loads of cycles"
    (reset-simulation)
    (check-sim-vs-reality)
    (mk-plan main-base)
    (check-eq? (length (plan-keys)) 6)
    (check-plan "vpc" plan-op-create?)
    (check-plan "vid" plan-op-create?)
    (check-plan "sn1" plan-op-create?)
    (check-plan "sn2" plan-op-create?)
    (apply-plan)
    (displayln "**** CHECKPOINT 1")
    (define oldids (check-sim-vs-reality))

    (mk-plan main-phase1)
    (check-eq? (length (plan-keys)) 5)
    (check-plan "vpc" plan-op-replace?)
    (check-plan "vid" plan-op-replace?)
    (check-plan "sn1" plan-op-replace?)
    (check-plan "sn2" plan-op-update?)
    (check-plan "inst1" plan-op-create?)

    (apply-plan)
    (displayln "**** CHECKPOINT 2")
    (define newids (check-sim-vs-reality))
    (check-id-changed "vpc" oldids newids)
    (check-id-changed "sn1" oldids newids)

    ;; planning the same should have no changes
    (mk-plan main-phase1)
    (check-eq? 0 (length (plan-keys)))
    (displayln "**** CHECKPOINT 3")
    (check-sim-vs-reality)

    (mk-plan main-base)
    (check-eq? (length (plan-keys)) 5)
    (check-plan "vpc" plan-op-replace?)
    (check-plan "vid" plan-op-replace?)
    (check-plan "sn1" plan-op-replace?)
    (check-plan "sn2" plan-op-update?)
    (check-plan "inst1" plan-op-delete?)
    (apply-plan)
    (displayln "**** CHECKPOINT 4")
    (define newids2 (check-sim-vs-reality))
    (check-id-changed "vpc" newids newids2)
    (check-id-changed "sn1" newids newids2)

    (mk-plan main-drivers)
    (check-eq? (length (plan-keys)) 4)
    (check-plan "vpc" plan-op-delete?)
    (check-plan "vid" plan-op-delete?)
    (check-plan "sn1" plan-op-delete?)
    (check-plan "sn2" plan-op-delete?)
    (apply-plan)
    (check-sim-vs-reality)
    )

   (test-case
    "IGW Simulation"

    (reset-simulation)

    (define (igw-barrier-match bcfg)
      (match bcfg
        [(barrier-cfg _ 'delete (resource 'beta _ _ _)) beta-instances-check]
        [_ #f]))

    (define (main-igw-sim)
      (:= svard svar-driver)
      (:= sim sim-driver)

      (:> vpc := sim :: alpha i1 "11.0.0.0/24")
      (::= vid svard svar (rref vpc id))
      (:> sn1 := sim :: gamma m1 "subnet" i1 (vid))
      (:> inst1 := sim :: delta m1 "instance" i1 (rref sn1 id))

      ;; simulates the IGW attachement to the VPC, which needs a pre-delete barrier that the instance is
      ;; gone before the attachment deletes - it doesn't have an explicit dependency on the instance

      (:> igwatt := sim :: beta m1 "igw1" vpc-id (vid))
      #t)

    (barrier-clear-all)
    (barrier-set 'pre igw-barrier-match)
    (barrier-set 'post gen-barrier-match)

    (check-sim-vs-reality)
    (mk-plan main-igw-sim)
    (check-eq? (length (plan-keys)) 7)
    (check-plan "vpc" plan-op-create?)
    (check-plan "vid" plan-op-create?)
    (check-plan "sn1" plan-op-create?)
    (check-plan "inst1" plan-op-create?)
    (check-plan "igwatt" plan-op-create?)
    (apply-plan)
    (displayln "**** CHECKPOINT IGW-1")
    (check-sim-vs-reality)
    (mk-plan main-drivers)
    (check-eq? (length (plan-keys)) 5)
    (check-plan "vpc" plan-op-delete?)
    (check-plan "vid" plan-op-delete?)
    (check-plan "sn1" plan-op-delete?)
    (check-plan "inst1" plan-op-delete?)
    (check-plan "igwatt" plan-op-delete?)
    (apply-plan)
    (displayln "**** CHECKPOINT IGW-2")
    (check-sim-vs-reality)
    )
   )
  )
 )
