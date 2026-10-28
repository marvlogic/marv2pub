#lang racket

(require
  (prefix-in g- graph)
  rackunit
  marv/graph
  rackunit/text-ui)

(require/expose marv/graph (tsort-delete))

(define (graph-fixture ophs)
  (define g (mk-graph
             '((sn1 vpc)
               (inst1 sn1)
               (assoc1 inst1)
               )))
  (for ([(k v) (in-hash ophs)])
    (graph-set-op! g k v))
  g)

(define (graph-fixture1 ophs)
  (define g (graph-fixture ophs))
  (graph-set-immutable! g 'sn1 'vpc)
  (graph-set-immutable! g 'inst1 'sn1)
  (graph-set-immutable! g 'assoc1 'inst1)
  g)

(define (graph-fixture2 ophs)
  (define g (graph-fixture ophs))
  (graph-set-immutable! g 'sn1 'vpc)
  (graph-set-immutable! g 'inst1 'sn1)
  g)

#;(exit
   (run-tests
    (test-suite
     "graph-execution"
     )))
(exit
 (run-tests
  (test-suite
   "graph operations"

   (test-case
    "simple path"
    (define ops (make-hash '((vpc . create) (sn1 . create) (inst1 . create) (assoc1 . create))))
    (define g (graph-fixture1 ops))
    (define gops (graph-get-operations g))
    (check-equal? (graph-operations gops) ops)
    (define ts (graph-tsort gops))
    (check-equal? ts '(vpc sn1 inst1 assoc1)))

   (test-case
    "manual graph construction, recreate sn1 and deps"
    (define g (mk-graph '()))
    (graph-add-edge! g 'sn1 'vpc)
    (graph-add-edge! g 'inst1 'sn1)
    (graph-add-edge! g 'assoc1 'inst1)
    (graph-set-op! g 'vpc 'noop)
    (graph-set-op! g 'sn1 'recreate)
    (graph-set-op! g 'inst1 'noop)
    (graph-set-op! g 'assoc1 'noop)
    (graph-set-immutable! g 'sn1 'vpc)
    (graph-set-immutable! g 'inst1 'sn1)
    (graph-set-immutable! g 'assoc1 'inst1)
    (check-true (g-dag? (graph-g g)))
    (graph-push! g)
    (define ops (graph-tsort (graph-get-operations g)))
    (check-equal? (graph-operations g)
                  (make-hash
                   '((vpc . noop) (sn1 . recreate) (inst1 . recreate) (assoc1 . recreate))))
    (check-equal? ops '((delete . assoc1) (delete . inst1) (delete . sn1) sn1 inst1 assoc1)))

   (test-case
    "recreate sn1 and deps"
    (define g (graph-fixture1 (hash 'sn1 'recreate)))
    (graph-push! g)
    (check-equal? (graph-operations g)
                  (make-hash
                   '((vpc . noop) (sn1 . recreate) (inst1 . recreate) (assoc1 . recreate))))
    (define ops (graph-tsort (graph-get-operations g)))
    (check-equal? ops '((delete . assoc1) (delete . inst1) (delete . sn1) sn1 inst1 assoc1)))

   (test-case
    "partial recreate"
    (define g (graph-fixture1 (hash 'sn1 'recreate 'assoc1 'delete)))
    (graph-push! g)
    (define ops (graph-tsort (graph-get-operations g)))
    (check-equal? (graph-operations g)
                  (make-hash
                   '((vpc . noop) (sn1 . recreate) (inst1 . recreate) (assoc1 . delete))))
    (check-equal? ops '((delete . assoc1) (delete . inst1) (delete . sn1) sn1 inst1)))

   (test-case
    "trigger re-create all resources from vpc recreate"
    (define g (graph-fixture1 (hash 'vpc 'recreate)))
    (graph-push! g)
    (define ops (graph-tsort (graph-get-operations g)))
    (check-equal? (graph-operations g)
                  (make-hash
                   '((vpc . recreate) (sn1 . recreate) (inst1 . recreate) (assoc1 . recreate))))
    (check-equal? ops '((delete . assoc1) (delete . inst1) (delete . sn1)  (delete . vpc) vpc sn1
                                          inst1 assoc1)))

   (test-case
    "empty operations"
    (define g (graph-fixture1 (hash)))
    ;; (graph-push! g)
    (define ops (graph-tsort (graph-get-operations g)))
    (check-equal? ops '()))

   (test-case
    "recreate inst1 and trigger update on assoc1"
    (define g (graph-fixture2 (hash 'inst1 'recreate)))
    (graph-push! g)
    (define ops (graph-tsort (graph-get-operations g)))
    (check-equal? ops '((delete . inst1) inst1 assoc1))
    (check-equal? (graph-operations g)
                  (make-hash
                   '((vpc . noop) (sn1 . noop) (inst1 . recreate) (assoc1 . update)))))

   (test-case
    "node with no dependents (sn2) (UNSTABLE!)"
    (define g (mk-graph '()))
    (graph-add-edge! g 'sn1 'vpc)
    (graph-add-edge! g 'inst1 'sn1)
    ;; (graph-add-edge! g 'sn2 'driver)
    (graph-add-vertex! g 'sn2 'create)
    (graph-set-op! g 'vpc 'create)
    (graph-set-op! g 'sn1 'create)
    (graph-set-op! g 'inst1 'create)
    (check-true (g-dag? (graph-g g)))
    ;; (graph-push! g)
    (define ops (graph-tsort (graph-get-operations g)))
    (check-equal? ops '(vpc sn1 inst1 sn2))
    (check-equal? (graph-operations g)
                  (make-hash
                   '((vpc . create) (sn1 . create) (inst1 . create) (sn2 . create)))))

   (test-case
    "graph task execution"
    (define g (mk-graph
               '((sn1 vpc) (sn2 vpc) (inst1 sn1) (assoc1 inst1)
                           (assoc2 inst1)
                           (inst1 vpc)
                           )))
    (define (runnable fs)
      ;; (displayln (vertex-state-blocked (graph-execute g fs)))
      (vertex-state-runnable (graph-execute g fs)))
    (check-equal? (runnable (set)) (set 'vpc))
    (check-equal? (runnable (set 'vpc)) (set 'sn1 'sn2))
    (check-equal? (runnable (set 'vpc 'sn1)) (set 'inst1 'sn2))
    (check-equal? (runnable (set 'vpc 'sn1 'sn2)) (set 'inst1))
    (check-equal? (runnable (set 'vpc 'sn1 'sn2 'inst1 'assoc1 'assoc2)) (set))
    (check-equal? (runnable (set 'vpc 'sn1 'sn2 'inst1 'assoc1 'assoc2)) (set)))

   (test-case
    "graph failed tasks"
    (define g (mk-graph
               '((sn1 vpc) (sn2 vpc) (inst1 sn1) (assoc1 inst1)
                           (assoc2 inst1)
                           (inst1 vpc)
                           )))
    (define (done-failed fs failed exp-run exp-blocked)
      (define res (graph-execute g fs failed))
      (check-equal? (vertex-state-runnable res) exp-run)
      (check-equal? (vertex-state-blocked res) exp-blocked))

    (done-failed (set 'vpc 'sn1 'sn2 'inst1) (set 'inst1) (set) (set 'assoc1 'assoc2))
    (done-failed (set 'vpc 'sn1 'sn2) (set 'sn1) (set) (set 'inst1 'assoc1 'assoc2))
    (done-failed (set 'vpc) (set 'vpc) (set) (set 'sn1 'sn2 'inst1 'assoc1 'assoc2))

    )

   (test-case
    "blocked vs free tasks"
    (define g (mk-graph
               '((sn1 vpc) (sn2 vpc) (inst1 sn1) (assoc1 inst1)
                           (assoc2 inst1)
                           (inst1 vpc)
                           )
               'delete))

    (graph-set-op! g 'vpc 'noop)

    (define (runnable finished)
      (vertex-state-runnable (graph-execute (graph-get-operations g) finished)))

    ;; (pretty-display (graph-get-delete-edges g))
    (check-equal? (runnable (set)) (set '(delete . assoc1) '(delete . assoc2) '(delete . sn2)))
    (check-equal? (runnable (set '(delete . assoc1) '(delete . assoc2) '(delete . sn2)))
                  (set '(delete . inst1) ))

    )

   (test-case
    "sequencing of deletes"
    (define g (mk-graph
               '((vid svard)
                 (vid vpc) (sn1 vid)
                 (sn1 sim) (sn2 vid)
                 (sn2 sim) (vpc sim))
               'delete))

    (define (runnable finished)
      (vertex-state-runnable (graph-execute (graph-get-operations g) finished)))

    ;; (pretty-display (graph-get-delete-edges g))
    (check-equal? (runnable (set)) (set '(delete . sn1) '(delete . sn2)))
    (check-equal? (runnable (set '(delete . sn1) '(delete . sn2))) (set '(delete . vid)))
    (check-equal? (runnable (set '(delete . sn1) '(delete . sn2) '(delete . vid)))
                  (set '(delete . vpc) '(delete . svard)))

    ))))

