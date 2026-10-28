#lang racket

(provide
 mk-graph
 (struct-out graph)
 (struct-out vertex-state)
 graph-push!
 graph-get-op
 graph-set-op!
 graph-add-vertex!
 graph-remove-vertex!
 graph-add-edge!
 graph-set-immutable!
 graph-execute
 graph-get-operations
 graph-get-neighbours
 graph-get-reverse-neighbours
 graph-get-edges
 graph-tsort
 )

(require graph)

(struct graph (g immutable-edges operations) #:prefab)

(define (delete-vtx s)
  (cons 'delete s))

(define (mk-graph edges [default-operation 'noop])
  (define g (unweighted-graph/directed edges))
  (graph g
         (mutable-set)
         (make-hash
          (for/list ([v (get-vertices g)])
            (cons v default-operation)))))

(define (graph-set-op! g vtx op)
  (hash-set! (graph-operations g) vtx op)
  g)

(define (graph-get-op g vtx)
  (hash-ref (graph-operations g) vtx 'noop))

(define (graph-set-immutable! g from to)
  (set-add! (graph-immutable-edges g) (list from to))
  g)

(define (graph-add-vertex! g vtx [default-operation 'noop])
  (add-vertex! (graph-g g) vtx)
  (graph-set-op! g vtx default-operation)
  g)

(define (graph-remove-vertex! g vtx)
  (remove-vertex! (graph-g g) vtx)
  (hash-remove! (graph-operations g) vtx)
  (define edges (graph-immutable-edges g))
  (for ([e (in-set edges)]
        #:when (member vtx e))
    (set-remove! edges e))
  g)

(define (graph-add-edge! g from to)
  (add-directed-edge! (graph-g g) from to)
  g)

(define (graph-push! grf)

  ;; TODO: need to sort out semantics; it returns the original graph but with the operations mutated
  ;; based on a transposition

  (define g (transpose (graph-g grf)))
  (define immutable-edges (graph-immutable-edges grf))
  (define operations (graph-operations grf))

  (define (operation v) (hash-ref operations v #f))
  (define (operation-set! v n) (hash-set! operations v n))

  (define (push-operation from to)
    (define frm-op (operation from))
    (define to-op (operation to))
    ;; (displayln (~a from ":" frm-op  " ->"  to ":" to-op " " immutable-edges))
    (define new-op
      (cond
        [(and (eq? 'recreate frm-op)
              (not (eq? 'delete to-op))
              (set-member? immutable-edges (list to from)))
         'recreate]
        [(and (eq? 'recreate frm-op)
              (not (eq? 'delete to-op)))
         ;; Not sure if pushing update is the way to go
         'update]
        [(eq? 'recreate to-op) 'recreate]
        [else to-op]))
    (operation-set! to new-op))

  (do-dfs g #:init (hash)
          #:prologue (from to) (push-operation from to)
          #:visit? (_f _t) #t)
  grf)

(define (get-delete-edges grf)

  (define g (transpose (graph-g grf)))
  (define operations (graph-operations grf))
  (define (operation v) (hash-ref operations v #f))

  ;; the vertex #f is still interesting to us, otherwise we will filter out terminal or stand-alone
  ;; vertices

  (define (is-used? v) (or (not v)(member (operation v) '(delete recreate))))

  (define (ccons from to acc)
    (if (and (is-used? from) (is-used? to))
        (cons (list (delete-vtx from) (delete-vtx to)) acc)
        acc))

  ;; we have to process-unvisited and use condition checks to see if the edge should be added.
  ;; Otherwise the algorithm filters out seen vertices that are used in other edges and we miss these
  ;; dependencies.

  (do-dfs g #:init null
          #:epilogue: (ccons $from $v $acc)
          #:process-unvisited?: #t
          #:process-unvisited: (ccons $from $to $acc)))

(define (tsort-delete grf)

  ;; Sort and filter the delete/recreate operations

  (define g (graph-g grf))
  (define operations (graph-operations grf))
  (define visited (mutable-set))
  (define (operation v) (hash-ref operations v #f))

  (define (ccons x acc)
    (set-add! visited x)
    (cons x acc))

  (define (visit v)
    (if (member (operation v) '(delete recreate))
        (not (set-member? visited v))
        #f))

  (do-dfs g #:init null
          #:epilogue: (ccons $v $acc)
          #:visit?: (visit $v)))

(struct vertex-state (runnable blocked) #:prefab)

(define (graph-execute grf finished [failed (set)])

  (define finished-ok (set-subtract finished failed))

  (define g (graph-g grf))

  (define (add-dep from to vts)
    (define blocked (vertex-state-blocked vts))
    (struct-copy
     vertex-state vts
     (blocked (if (or (not from) (set-member? finished-ok to))
                  blocked
                  (set-add blocked from)))))

  (define (can-execute? v vts)
    (define runnable (vertex-state-runnable vts))
    (define blocked (vertex-state-blocked vts))
    (struct-copy
     vertex-state vts
     (runnable
      (if (or (set-member? finished-ok v)(set-member? blocked v))
          runnable
          (set-add runnable v)))))

  ;; We have to use 'process-unvisited' otherwise the search will filter out a vertex that has been
  ;; 'seen' already, and this will skip other edges that target that vertex. The prologue calls are
  ;; done on the way down into the graph and the epilogues are done on the return. So by the time the
  ;; epilogue is called for an edge we already know if its blocked by dependencies.

  (define vts
    (do-dfs g #:init (vertex-state (set) (set))
            #:prologue: (add-dep $from $to $acc)
            #:epilogue: (can-execute? $v $acc)
            #:process-unvisited?: #t
            #:process-unvisited: (add-dep $from $to $acc)))
  (vertex-state (set-subtract (vertex-state-runnable vts) failed) (vertex-state-blocked vts)))

(define (graph-get-operations grf)

  (define ops-graph (graph-copy (graph-g grf)))

  ;; TODO: I suspect tsort-delete can be retired in favour of get-delete-edges
  (define deletes (tsort-delete grf))
  (define delete-edges (get-delete-edges grf))

  (for ([v (get-vertices ops-graph)]
        #:when (member (graph-get-op grf v) '(delete noop)))
    (remove-vertex! ops-graph v))

  (define recreate-edges
    (for/list ([x deletes]
               #:when (member (graph-get-op grf x) '(recreate noop update)))
      (list x (delete-vtx x))))

  (define new-edges (append delete-edges recreate-edges (get-edges ops-graph)))
  (define newg (unweighted-graph/directed new-edges))

  ;; we end up with an edge from #f to terminal nodes, which we don't need...
  (remove-vertex! newg '(delete . #f))

  ;; force all the original vertices back into the new graph, even if they didn't have any edge to
  ;; them originally
  (for ([v (get-vertices ops-graph)])(add-vertex! newg v))
  (struct-copy graph grf (g newg)))

(define (graph-get-neighbours g v)
  (get-neighbors (graph-g g) v))

(define (graph-get-reverse-neighbours g v)
  (get-neighbors (transpose (graph-g g)) v))

(define (graph-tsort grf)
  ;; for sequencing operations we need the reverse topo-sort of the graph
  (tsort (transpose (graph-g grf))))

(define (graph-get-edges grf)
  (get-edges (graph-g grf)))
