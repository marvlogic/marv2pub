#lang racket

(provide
 (for-syntax
  define/prefix-resource))

(require
  marv/logger
  marv/resources
  marv/model
  (for-syntax racket/syntax)
  )

(define-for-syntax (define/prefix-resource type-lookup mk-comp-resource model-mut stx)

  ;; TODO: why oh why?
  (define (symbol-or-false r)
    (if (eq? #f (syntax-e r)) r #`'#,(format-symbol "~a" r)))

  (syntax-case stx ()
    [(_ (res-type cmd) (create element modify) handler ...)
     (with-syntax
         ([model-id (format-id stx "~a-model" #'res-type)]
          [handler-id (format-id stx "~a-hdlr" #'res-type)]
          [crs (symbol-or-false #'create)]
          [els (symbol-or-false #'element)]
          [mods (symbol-or-false #'modify)]
          [tlu type-lookup]
          [model-mut model-mut]
          [mk-comp-resource mk-comp-resource]
          )
       #'(begin
           (define model-id (model-mut crs els mods))
           (define (((res-type cfg) drv)id)
             (mk-comp-resource 'res-type id drv crs els cfg #f mods))
           (define (handler-id cmd)
             (log-marv-debug (~a "resource handler: " 'res-type " cmd: " cmd))
             (cond
               [(op-model? cmd) (model-attrs-all model-id)]
               [else
                handler ...]))
           (hash-set! tlu 'res-type handler-id)
           (provide res-type))
       )]
    ))
