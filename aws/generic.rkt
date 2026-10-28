#lang racket

(provide
 generic-create
 generic-read
 generic-readx
 generic-update)

(require
  marv/model
  marv/resources
  (for-syntax racket/syntax))

(define (generic-create model type cfg api-create-fn
                        #:mdl->api-fn [mdl->api-fn values]
                        #:api->mdl-fn [api->mdl-fn values]
                        #:update-hdlr [update-hdlr (lambda(op)(raise(~a "no update handler for " op)))])
  (define result-cfg ((compose1 api->mdl-fn api-create-fn mdl->api-fn) cfg))
  (define diff (model-diff-hs model result-cfg cfg))
  (if (hash-empty? diff)
      result-cfg
      (update-hdlr (op-update type result-cfg (op-diff-result diff #f)))))

(define (generic-update type cfg diff api-update-fn mdl->api-fn read-hdlr)
  ((compose1 api-update-fn mdl->api-fn) cfg)
  (read-hdlr(op-read type cfg)))

(define (generic-readx cfg api-read-fn
                       #:mdl->api-fn [mdl->api-fn values]
                       #:api->mdl-fn [api->mdl-fn values] )
  ((compose1 api->mdl-fn api-read-fn mdl->api-fn) cfg))

;; TODO: retire this approach and use generic-readx

(define (generic-read-helper describe-api-cmd id-key filter-key result-key cfg)

  ;; Not using the API module to build the filter, but if we were then it would look like this:
  ;; (define req (describe-req #:filters (list ( aws-filter #:name id-key #:values (list (hash-ref cfg filter-key))))))
  ;;
  ;; (But they should all yield the same basic structure)

  (define req (hash 'filters (list ( hash 'name id-key 'values (list (hash-ref cfg filter-key))))))
  (define result (describe-api-cmd req))
  (car (hash-ref result result-key)))

(define-syntax (generic-read stx)
  (syntax-case stx ()
    ([_ type cfg]
     (with-syntax
         ([describe-cmd (format-id stx "describe-~as" #'type)]
          ;; [describe-req (format-id stx "aws-describe-~as-request" #'type)]
          [id-key (format "~a-id" (syntax-e #'type))]
          [filter-key (format-symbol "~a-id" (syntax-e #'type))]
          [result-key (format-symbol "~as" (syntax-e #'type))])
       #'(generic-read-helper describe-cmd id-key 'filter-key 'result-key cfg)))
    ([_ type filter-key cfg]
     (with-syntax
         ([describe-cmd (format-id stx "describe-~as" #'type)]
          [id-key (format "~a" (syntax-e #'filter-key))]
          [result-key (format-symbol "~as" (syntax-e #'type))]
          )
       #'(generic-read-helper describe-cmd id-key 'filter-key 'result-key cfg)))))

