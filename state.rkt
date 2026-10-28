#lang racket

(provide
  := :: ::= :>
  <=
  module-ctx
  xref
  state-init
  state-read
  state-dump
  state-repr
  state-ref
  state-keys
  state-for-each
  state-set
  state-in?
  state-load
  state-save
  state-edit
  state-resources
  refresh-resource-state
  plan-display
  plan-apply
  plan-init
  plan-ref
  plan-update
  plan-keys
  for/ctx
  for/res
  plan-ref-res
  (struct-out plan-op)
  (struct-out plan-op-none)
  (struct-out plan-op-create)
  (struct-out plan-op-update)
  (struct-out plan-op-replace)
  (struct-out plan-op-delete)

  report-filter
  with-ctx-assign
  with-ctx)

(require
  racket/hash
  (for-syntax syntax/parse)
  syntax/parse
  marv/graph
  marv/logger
  marv/utils
  marv/tasks
  marv/barrier
  (prefix-in g- graph)
  marv/resources)


;; All the plan-op structures hold the specification of the item, except for the delete operation
;; which holds the item current state (because it won't exist in the spec)

(struct plan-op (resource))
(struct plan-op-none plan-op ())
(struct plan-op-create plan-op ())
(struct plan-op-update plan-op (diff))
(struct plan-op-replace plan-op (old-res))
(struct plan-op-delete plan-op ())

;; A plan-report is generated for each item of a plan and output in a hash, at the end of a
;; plan-apply. Statuses are:
;;   'pending
;;   'done
;;   'delete-done
;;   'failed

(define (mk-init-report plan)
  (for/hash ([(k _) (in-hash plan)])
    (values k 'pending)))

(define (reset-report plan-report)
  (hash-map/copy plan-report
                 (lambda (k v)
                   (values k (if (eq? 'failed v) 'pending v)))))

(define (report-update rpt id new-status) (hash-set rpt id new-status))
(define (reported-done? rpt id) (eq? 'done (hash-ref rpt id)))
(define (reported-incomplete? rpt id) (not (eq? (hash-ref rpt id) 'done)))
(define (report-filter rpt st) (hash-filter rpt (lambda (_ v) (eq? st v))))

(define (mk-id ctx) (string-join (reverse ctx) "."))

;; statectx holds 2 or 3 values in a list:
;; - boolean: legal? - is true when within a resource context
;; - listof strings: reverse list of ids to build resource id with -  e.g. ("myid" "main")
;; - string: attr-id - may hold the id of the attribute *within* the resource being assigned to
;;   (if the <= operator has been used)

(define statectx (make-parameter '(#f ())))

;; returns the validity of the current context; resources can only be manipulated if the correct
;; context is available.
(define (statectx-legal?) (car (statectx)))

;; returns list of ids, joined together they make the resource id at the point of :=
(define (statectx-res-path) (cadr (statectx)))
(define statectx-res-id (compose1 mk-id statectx-res-path))

;; returns the attribute of the resource that is currently being assigned to under the <= operator
(define (statectx-attr-id) (caddr (statectx)))

(define-syntax-rule (statectx-change legal? id expr1 expr ...)
  (parameterize
    ([statectx
      (list legal?
            (cons
              (if (symbol? id) (symbol->string id) id)
              (statectx-res-path)))])
    expr1
    expr ...))

;; (define-syntax-rule (module-ctx state-loc expr0 expr1 ...)
;;   (parameterize
;;       ([statectx (list #t '())])
;;     (log-marv-info (~a "Entering state context for " state-loc))
;;     (state-load-parameterize
;;      state-loc
;;      (lambda() expr0 expr1 ...))))

(define-syntax-rule (with-ctx id expr0 expr ...)
  (statectx-change #f id expr0 expr ...))

(define (state-ctx-push id)
  (statectx
    (list #t
          (cons
            (if (symbol? id) (symbol->string id) id)
            (statectx-res-path)))))

(define (state-ctx-pop)
  (statectx (list #f (rest (statectx-res-path)))))

(define-syntax-rule (with-ctx-assign id expr0 expr ... exprx)
  (define id
    (statectx-change #t 'id expr0 expr ... (lambda () exprx))))

(define-syntax-rule (with-attr-assign attr-id expr0)
  (parameterize ([statectx (list #t (statectx-res-path) (symbol->string attr-id))])
    (log-marv-debug (~a "attribute ctx:" (statectx)))
    (cons attr-id expr0)))

(define (state-ctx-handler lhs rhs srcfile srcline)
  (statectx-change
    #t lhs
    (define state-id (statectx-res-id))
    (define srcref (format "~a:~a" (find-relative-path (current-directory) srcfile) srcline))
    (log-marv-info (format "Defining ~a at ~a" state-id srcref))
    (state-srcloc-set state-id srcref)
    ;; rhs is a zero-param lambda - we call that to get a function that accepts an id and finally
    ;; creates a resource
    (define idfunc (apply rhs '()))
    (define res (apply idfunc (list state-id)))

    (when (resource? res)
      (log-marv-debug (~a "resource: " res))
      (log-marv-debug (~a "model: "
                          (apply-resource-driver-func res (op-model (resource-type res) null)))))

    (define in-state? (plan-check-add state-id res))

    (case-lambda
      ;; This is the function that gets called whenever another resource references this resource.
      [()
       (define legal? (statectx-legal?))
       (define this-state-id (statectx-res-id))
       (cond
         [legal?

          (log-marv-debug (format "Dependency: ~a -> ~a" this-state-id state-id))

          ;; TODO: not sure about how to manage drivers, should be in state as a first-class type there
          ;; or outside of state altogether? For now, not adding a dependency to a driver
          (graph-add-edge! (state-graph) this-state-id state-id)

          (cond
            [(resource? res)
             ((compose1 computation-hs resource-cfg state-ref) state-id)]
            [(driver? res) res]
            [else (log-marv-warn (~a "WARNING: unreliable!" res)) res])]
         [else
          (raise (format "not in assignment context ~a -> ~a" this-state-id state-id))
          (resource-cfg res)])]
      [(param1)
       (define legal? (statectx-legal?))
       (define this-state-id (statectx-res-id))
       (unless legal? (log-marv-warn (~a "CAUTION: called " state-id " outside context!")))
       (graph-add-edge! (state-graph) this-state-id state-id)
       (log-marv-info (format "Dependency: ~a -> ~a" this-state-id state-id))
       (case param1
         [(res) res]
         [(type) (resource-type res)]
         [(id)
          (log-marv-info "WWW")
          (cond
            [(resource? res) (resource-id res)]
            [(driver? res) (driver-id res)])]
         [else res]
         )])
    ))

(define (apply-resource-driver-func res op)
  (((compose1 driver-func state-ref resource-drvr) res) op))

(define (do-resource-diff id given-res)
  (define current-res (state-ref id))

  ;; used to be resolve-computations/filter but that stopped dependencies from re-creating when an
  ;; immutable attribute changed. I think we need to leave unresolved computations in given-cfg so
  ;; that they trigger differences in model-diff-hs.
  ;; TODO: put testing around this as well as model-diff-hs.

  (define given-cfg (resolve-computations (resource-cfg given-res)))
  (define diff
    (apply-resource-driver-func
      current-res
      (op-diff (resource-type current-res)
               (computation-hs (resource-cfg current-res)) given-cfg)))

  (define new-res (struct-copy resource current-res [cfg (computation given-cfg)]))
  (if (op-diff-result-immutable-change? diff)
      (plan-op-replace given-res current-res)
      (if (hash-empty? (op-diff-result-hs diff))
          (plan-op-none new-res)
          (plan-op-update new-res diff))))

(define (refresh-resource-state id)
  (log-marv-info (~a "refreshing " id))
  (define state-res (state-ref id))
  (define cfg (resolve-computations/filter (resource-cfg state-res)))
  (define upd-cfg (apply-resource-driver-func state-res (op-read (resource-type state-res) cfg)))
  (state-set id (struct-copy resource state-res [cfg (computation upd-cfg)])))

(define (plan-check-add id spec)
  ;; Returns #t if the resource is already in state, #f otherwise
  (cond
    [(and (driver? spec) (not (state-in? id)))
     (log-marv-warn "Driver Lifecycle not implemented")
     ;; setup an initialised driver that has its lambda in place, and remove it from the plan
     (plan-ws-set id (plan-op-create spec))
     #t]
    [(and (driver? spec) (state-in? id))
     (log-marv-warn "Driver Lifecycle not implemented")

     ;; TODO: bit of a smell... force the spec to overwrite state - because when state is loaded from
     ;; file, then the driver lambda isn't present
     (state-set id spec)

     ;; TODO: another smell; initial plan is 'delete everything'
     (plan-remove id)
     (graph-set-op! (state-graph) id 'noop)
     #t]
    [(and (state-in? id) (resource? spec) (eq? (resource-type spec) (resource-type (state-ref id))))

     ;; Working out if the resource is different to state, if it's the same type

     (log-marv-info (format "EXISTS: ~a" id))

     ;; when the resource cmptn is resolved in refresh-resource-state it reads from state; it's just
     ;; a bit of a long winded way to do it.

     (refresh-resource-state id)
     (define op (do-resource-diff id spec))
     (log-marv-debug (~a "diff result: " op (plan-op-resource op)))
     (plan-ws-set id op)
     (log-marv-debug (state-ref id))
     #t]
    [(and (resource? spec) (state-in? id))
     ;; TODO: replacing a resource, same name different type, should plan delete old/create new
     (log-marv-info (format "REPLACE: ~a" id))
     (graph-add-vertex! (state-graph) id 'recreate-new)
     (raise "replacement with new type is unimplemented")
     #t]
    [else
     (log-marv-info (format "ADD: ~a" id))
     (plan-ws-set id (plan-op-create spec))
     #f]))

(define-syntax-rule (xref r attr)
  (let ([tgt-id (r 'id)]
        [tgt-attr (format "~a.~a" (r 'id) 'attr)]
        [rsid (statectx-res-id)]
        [myattr (statectx-attr-id)])
    (log-marv-debug (~a "xref from " rsid "." myattr " to " tgt-attr))
    (hash-ref (r) 'attr)))

(define-syntax (<= stx)
  (syntax-parse stx
    [(_ (~seq a b) ...) #'(make-immutable-hash (list (with-attr-assign 'a b) ...))]))

(define for/res 'for/res)
;; (define-literal-set marv-lits (for/res))

(define-syntax (:= stx)
  (syntax-parse stx
    #:literals (for/res)
    [(_ lhs rhs1)
     (with-syntax
       ([file (syntax-source stx)]
        [line (syntax-line stx)])
       #'(begin
           (define lhs ((ctxhandler) (symbol->string 'lhs) (lambda () rhs1) file line))
           ))]
    [(_ id for/res clauses body ...)
     #'(define id (for/ctx id := clauses body ...))]
    ))

;; Returns a partial applied entity that only needs an Id to be completed.
(define-syntax-rule (:: drv type cfg0 cfg ...)
  ((type cfg0 cfg ...) (drv)))

(define-syntax-rule (::= var drv type cfg0 cfg ...)
  (:= var (:: drv type cfg0 cfg ...)))

(define-syntax (:> stx)
  (define-splicing-syntax-class keyvals (pattern (~seq k:id v:expr)))
  (syntax-parse stx
    #:literals (:= ::)
    [(MACRO var := drv :: type fs:keyvals ...)
     (syntax/loc stx (MACRO var drv type (~@ fs.k fs.v) ...))]
    [(_ var drv type fs:keyvals ...)
     (syntax/loc stx (:= var (:: drv type (hash (~@ 'fs.k fs.v) ...))))]))

(define-syntax-rule (with-> p1 (var rhs1 rhs ...) ...)
  (begin
    (define var ((ctxhandler) (symbol->string 'var) (lambda () (curry rhs1 rhs ... p1)) null null)) ...
    (list var ...)
    ))

(define-syntax (for/ctx stx)
  (syntax-parse stx
    #:literals (:=)
    [(MACRO res-id := (for-clause ...) body ...)
     #'(with-ctx (symbol->string (syntax-e #'res-id))
                 (MACRO res-id (for-clause ...) body ...))]
    [(_ res-id (for-clause ...) body ...)
     #'(for/list (for-clause ...)
         (with-ctx (~a res-id)
                   body ...))]
    ))

;; Not sure where I'm going with this... yet.
(define-syntax-rule (group-with-> p1 (var rhs1 rhs ...) ...)
  (let* ([var ((ctxhandler) (symbol->string 'var) (lambda () (curry rhs1 rhs ... p1)) null null)] ...)
    (list var ...)
    ))

;; What interfaces would context-handler support (apart from :=)?
(define ctxhandler (make-parameter state-ctx-handler))
(define state (make-parameter (hash)))
(define state-graph (make-parameter (mk-graph null)))
(define state-srcloc (make-parameter (hash)))
(define state-locset (make-parameter '()))

(define module-ctx (make-parameter '()))
(define module-graph (make-parameter (mk-graph null)))

(define (state-init)
  (state-graph (mk-graph null))
  (module-graph (mk-graph null))
  (state-srcloc (hash))
  (state (hash)))

(define (state-init-parameterize loc thk)
  (parameterize
    ([state-graph (mk-graph null)]
     [state-srcloc (hash)]
     [state (hash)]
     [plan (hash)])
    (define res (thk))
    (state-locset (cons (list loc (cons (state-repr) (plan))) (state-locset)))
    res))

(define (state-in? id) (hash-has-key? (state) id))

(define (state-set id ns)
  (state (hash-set (state) id ns)))

(define (state-create id ns)
  (log-marv-info (~a "creating in state: " id))
  (state-set id ns))

(define (state-ref id) (hash-ref (state) id))
(define (state-keys) (hash-keys (state)))
(define (state-remove id)
  (log-marv-info (~a "removing from state: " id))
  (graph-remove-vertex! (state-graph) id)
  (state-srcloc (hash-remove (state-srcloc) id))
  (state (hash-remove (state) id)))

(define (state-srcloc-ref id) (hash-ref (state-srcloc) id))
(define (state-srcloc-set id locn) (state-srcloc (hash-set (state-srcloc) id locn)))

(define (state-for-each proc)
  (for/hash ([(k v) (in-hash (state))] #:when (resource? v))
    (proc k v)))

(define (state-resources)
  (define (unwrap v) (struct-copy resource v [cfg (computation-hs (resource-cfg v))]))
  (map unwrap (filter resource? (hash-values (state)))))

(define (state-read)
  (define all (read))
  (state
    (hash-apply
      (hash-ref all 'resources)
      (lambda (_ v) (if (resource? v) (struct-copy resource v [cfg (computation
                                                                     (resource-cfg v))]) v))))
  (state-graph (mk-graph (hash-ref all 'graph) 'delete))
  (state-srcloc (hash-ref all 'srcloc (hash))))

(define (state-read-parameterize loc thk)
  (define all (read))
  (parameterize
    ([state
      (hash-apply
        (hash-ref all 'resources)
        (lambda (_ v) (if (resource? v) (struct-copy resource v [cfg (computation
                                                                       (resource-cfg v))]) v)))]
     [state-graph (mk-graph (hash-ref all 'graph) 'delete)]
     [state-srcloc (hash-ref all 'srcloc (hash))]
     [plan (hash)])
    (plan-init)
    (define res (thk))
    ;; (state-locset (hash-set (state-locset) loc (cons (state) (plan))))
    (state-locset (cons (list loc (cons (state-repr) (plan))) (state-locset)))
    res))

(define (state-edit id)
  (define edfile (make-temporary-file))
  (with-output-to-file #:exists 'replace edfile
    (lambda () (pretty-print (state-ref id))))
  ;; TODO: Make this use $EDITOR env var
  (when (system (~a "nvim " edfile))
    (state-set id
               (with-input-from-file edfile (lambda () (read))))))

(define (state-save f)
  ;; TODO: raise violation if it contains unresolved computations
  (with-output-to-file #:exists 'replace f
    (lambda () (state-dump))))

(define (state-load f)
  (if (file-exists? f)
      (with-input-from-file f (lambda () (state-read)))
      (state-init)))

(define (state-load-parameterize loc thk)
  (if (file-exists? loc)
      (with-input-from-file loc (lambda () (state-read-parameterize loc thk)))
      (state-init-parameterize loc thk)))

(define (state-repr)
  (hash
    'resources (hash-map/copy
                 (state)
                 (lambda (k v)
                   (values k
                           ;; we can't store procedures (driver-func) so we need to set them to
                           ;; null before persistence.
                           (cond
                             [(driver? v) (struct-copy driver v [func null])]
                             [(resource? v) (struct-copy resource v [cfg (computation-hs (resource-cfg v))])])
                           )))
    'graph (graph-get-edges (state-graph))
    'srcloc (state-srcloc)))

(define (state-dump) (pretty-write (state-repr)))

(define (plan-apply #:ids (plan-ids (plan-keys)) #:prior-report (prior-report #f))
  (log-marv-info "##### PLAN APPLICATION:")
  (log-marv-info (~a "Applying Plan IDs: " plan-ids))
  (log-marv-info (~a " prior report:" prior-report))
  (log-marv-info (pretty-display (state-locset)))
  (define (barrier-check-pre bcfg contn) (barrier-find-first 'pre bcfg contn))
  (define (barrier-check-post bcfg contn) (barrier-find-first 'post bcfg contn))

  (define plan-reports (if prior-report prior-report (mk-init-report (plan))))

  (log-marv-info (~a "Reports: " plan-reports))

  (define (create k r)
    (log-marv-info (~a "CREATING " k))
    (log-marv-debug (~a " res: " r))
    (match r
      [(driver _ _ _ _)
       (log-marv-info "driver!")
       (state-set k r)]
      [(resource type _ _d cmptn)
       (define cfg (resolve-computations/filter cmptn))
       (log-marv-debug (~a "resolved: " cfg))
       (define result (apply-resource-driver-func r (op-create type cfg)))
       (log-marv-debug (~a "result: " result))
       (state-create k (struct-copy resource r [cfg (computation result)]))
       ]))

  (define (update k newr diff)
    (log-marv-info (~a "UPDATING " k))
    (log-marv-debug (~a "updating " k (op-diff-result-hs diff)))
    (match newr
      [(driver _ _ _ _)
       (log-marv-info "driver!")
       (state-set k newr)]
      [(resource _ _ _ cmptn)
       (define resolved-res (resolve-computations/filter cmptn))
       (define result (apply-resource-driver-func
                        (state-ref k)
                        (op-update (resource-type newr) resolved-res diff)))
       (log-marv-debug (~a "result: " result))
       (state-set k (struct-copy resource newr [cfg (computation result)]))]
      ))

  (define (delete k)
    (define current-res
      (if (plan-op-delete? (plan-ref k))
          (plan-op-resource (plan-ref k))
          (plan-op-replace-old-res (plan-ref k))))
    (log-marv-info (~a "DELETING " k))
    (log-marv-debug (~a "   " current-res))

    ;; FIXME: there's a bug if the resource removal doesn't work; it stays in state
    ;; but during a replacement the replacement operation (create) isn't cancelled

    (define result
      (match current-res
        [(driver _ _ _ _) null]
        [(resource type _i _d cmptn)
         (log-marv-info "deleting")
         (log-marv-debug cmptn)
         (define cfg (resolve-computations/filter cmptn))
         (apply-resource-driver-func current-res (op-delete type cfg))]))
    (log-marv-debug (~a "delete result: " result)))

  (define (clean-up-failed-apply e)
    (log-marv-info "ERROR during apply, cleaning up:")
    (log-marv-info (plan-keys))
    (for ([k (plan-keys)])
      (when (plan-op-create? (plan-ref k))
        (state-remove k))
      (raise-result-error 'plan-application "success" e)))

  (define (set-failed-plan-item k)
    ;; TODO: removing things from state feels wrong, seems like a missing
    ;; structure to handle the interim process of creating a new state is required
    (when (plan-op-create? (plan-ref k))
      (state-remove k))
    (set! plan-reports (report-update plan-reports k 'failed)))

  ;; TODO: somewhere around here needs to resolve the work-to-be-done using the plan, the
  ;; plan-reports and state where necessary.

  (with-handlers ([exn:fail? clean-up-failed-apply])

    (define ops-done (list->mutable-set (hash-keys (report-filter plan-reports 'done))))
    (define ops-failed (mutable-set))

    (define (task-exn-handler task-id e)
      (printf "*** Exception in task id ~a: ~a\n" task-id e)
      (pretty-display (plan))
      (pretty-display (state-keys))
      (printf "*** End exception report\n")
      (define k (if (pair? task-id) (cdr task-id) task-id))
      (set-failed-plan-item k)
      (log-marv-info plan-reports)
      (set-add! ops-failed task-id)
      )

    (define (final-delete delk)
      (define k (cdr delk))
      (set! plan-reports (report-update plan-reports k 'done))
      (when (plan-op-delete? (plan-ref k))
        (state-remove k))
      (log-marv-info (~a "final-delete finished: " k))
      #t)

    (define (mk-delete-task dk core-fn)

      ;; makes up the chain of continutations for doing the deletion for item k. The sequence is:
      ;;   barrier-check-pre -> core-fn -> barrier-check-post -> final-delete(item)

      (define k (cdr dk))
      (define pop (plan-ref k))
      (define res (if (plan-op-replace? pop)
                      (plan-op-replace-old-res pop)
                      (plan-op-resource pop)))
      (define bcfg (barrier-cfg k 'delete res))

      (define (task-fn)
        (barrier-check-pre
          bcfg
          (lambda ()
            (core-fn)
            (barrier-check-post
              bcfg
              (lambda ()
                (set-add! ops-done dk)
                (final-delete dk)
                (log-marv-info (~a "task complete: " dk)))))))
      (task dk task-fn task-exn-handler))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; Create all the plan tasks

    (define ops-graph (graph-get-operations (state-graph)))
    (log-marv-info (~a "Ops graph: " ops-graph))
    (log-marv-info (~a "  oper-edges:" (g-get-edges (graph-g ops-graph))))
    (log-marv-info (~a "  tsorted:" (graph-tsort ops-graph)))
    (log-marv-info (~a "  stateedges:" (g-get-edges (graph-g (state-graph)))))

    (define (mk-op-task k op core-fn)

      ;; makes up the chain of continutations for doing the work for item k. The sequence is:
      ;;   barrier-check-pre -> core-fn -> barrier-check-post -> update plan-reports(item)

      (define pop (plan-ref k))
      (define bcfg (barrier-cfg k op (plan-op-resource pop)))

      (define (task-fn)
        (barrier-check-pre
          bcfg
          (lambda ()
            (core-fn)
            (barrier-check-post
              bcfg
              (lambda ()
                ;; (when (plan-op-delete? (plan-ref k)) (final-delete k))
                ;;FIX:
                (set! plan-reports (report-update plan-reports k 'done))
                (set-add! ops-done k)
                (log-marv-info (~a "task complete: " k)))))))
      (task k task-fn task-exn-handler))

    (define set->immutable (compose1 list->set set->list))
    (define (run-cycle finished failed)
      (define g-vts (graph-execute ops-graph (set-union finished failed) failed))
      (define runnable-tasks (set-intersect
                               (vertex-state-runnable g-vts)
                               (set-union (list->set plan-ids)
                                          (list->set (map (lambda (c) (cons 'delete c)) plan-ids)))))
      (log-marv-info (~a "run-cycle: " g-vts " done:" finished " failed:" failed))
      (log-marv-info (~a "runnables: " runnable-tasks))
      (cond
        [(set-empty? runnable-tasks)
         (log-marv-info (~a "no more tasks to run, setting blocked items to failed:"
                            (vertex-state-blocked g-vts)))
         (for ([b (vertex-state-blocked g-vts)])
           (define k (if (pair? b) (cdr b) b))
           (set-failed-plan-item k))]
        [else
         (run-tasks
           (for/list ([op runnable-tasks])
             (log-marv-debug (format "Queue task for ~a" op))
             (define (plan-ref?? k) (if (plan-ref? k) (plan-ref k) #f))
             (match op
               [(cons 'delete k) (mk-delete-task op (lambda () (delete k)))]
               [k (match (plan-ref?? k)
                    [(plan-op-create r) (mk-op-task k 'create (lambda () (create k r)))]
                    [(plan-op-replace new-res _) (mk-op-task k 'create (lambda () (create k new-res)))]
                    [(plan-op-update newr diff) (mk-op-task k 'update (lambda () (update k newr diff)))]
                    [#f (log-marv-warn "noop - skipped") #f]
                    [_ (raise-argument-error 'invalid-plan-op "unknown/no-op" (plan-ref k))])])
             ))
         (log-marv-info (~a "tasks finished: " ops-done " tasks failed: " ops-failed))
         (run-cycle (set->immutable ops-done) (set->immutable ops-failed))])
      )
    (run-cycle (set->immutable ops-done) (set->immutable ops-failed)))

  (log-marv-info (~a "Final reports: " plan-reports))
  plan-reports)

(define plan (make-parameter (hash)))
(define (plan-init)
  ;; initial plan is "delete everything". then as plan is built the deleted items get over-written
  ;; leaving behind anything that currently isn't in state
  (plan (hash-map/copy (state) (lambda (k v) (values k (plan-op-delete v))))))

(define (plan-display)
  (pretty-write (plan)
                #; (map (lambda (k) (cons k (plan-op (plan-ref k)))) (plan-keys))))

(define (plan-ws-set i op)
  (cond
    [(plan-op-none? op)
     (graph-set-op! (state-graph) i 'noop)
     (plan-remove i)]
    [else
     (define vertex-op
       (cond
         [(plan-op-create? op) 'create]
         [(plan-op-update? op) 'update]
         [(plan-op-replace? op) 'recreate]))
     (graph-add-vertex! (state-graph) i vertex-op)

     ;; TODO: redundant to store it in state AND plan? Get from state via plan-ref-res?
     (state-set i (plan-op-resource op))
     (plan (hash-set (plan) i op))]))

(define (plan-ref i) (hash-ref (plan) i))

(define (plan-update i update-fn)

  ;; Update the configuration of a resource in the plan, used during interactive resolution

  (match (plan-ref i)
    [(plan-op-create r)
     (plan (hash-set (plan) i (plan-op-create (resource-update-cfg r update-fn))))]
    [_ void]))

(define (plan-ref? i) (hash-has-key? (plan) i))
(define (plan-remove i) (plan (hash-remove (plan) i)))
(define (plan-keys) (hash-keys (plan)))
(define plan-ref-res (compose1 plan-op-resource plan-ref))

