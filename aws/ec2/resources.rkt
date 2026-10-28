#lang racket

#|

This module implements a scheme which maps model attributes onto the request attributes, when issuing
create or modify requests, and from the result/response attributes back to the model attributes.

In AWS, there are usually request and result shapes  used in the API requests. So the initial model
can be a union of the two shapes, unless conflicts occur and this will require manual resolution.

|#

(require
  racket/hash
  marv/model
  marv/utils
  marv/resources
  marv/logger
  marv/aws/generic
  marv/aws/ec2/resources-base
  marv/aws/ec2/boto
  marv/aws/ec2/ops
  )

;; TODO: id parameters seem to be unused - and yet; the driver could support forms of replacement
;; references e.g. "$id". This may allow e.g. tags to contain references to the controlling marv
;; resource

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Utility functions for model<->api transformations. Mostly returning single-parameter lambda
;; functions that operate on config; they can be composed by the operations functions.

(define (sub-result r) (lambda(cfg)(hash-ref cfg r)))

(define (merge-exclusive cfg) (lambda(res) (hash-union cfg res)))

(define (diff->generic-update r diff)
  (define (wrap-update-req-attrs req) (hash-apply req (lambda(_ v)(hash 'value (attr-op-val v)))))
  (lambda(cfg) (hash-set (wrap-update-req-attrs (op-diff-result-hs diff)) r (hash-ref cfg r))))

(define (hash-rename-key hs oldk newk) (hash-remove (hash-set hs newk (hash-ref hs oldk)) oldk))

(define (rename-key oldk newk) (lambda(cfg) (hash-rename-key cfg oldk newk)))

(define (add-cfg* . args) (lambda(cfg) (apply hash-set* cfg args)))

(define (remove-cfg k) (lambda(cfg) (hash-remove cfg k)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; TODO: Adding a new ec2 resource
;;

(define/res (aws-key-pair cmd) (create-key-pair-request key-pair #f)
  (match cmd
    [(op-create type cfg) (generic-create aws-key-pair-model type cfg create-key-pair)]
    [(op-read _ cfg) (generic-read key-pair cfg)]
    [(op-diff _ oldcfg newcfg) (generic-diff aws-key-pair-model oldcfg newcfg)]
    [(op-delete _ cfg) (delete-key-pair cfg)]
    [_ (log-marv-warn (~a "ignoring unknown command:" cmd))
       cmd]))

(define/res (aws-vpc cmd) (create-vpc-request vpc modify-vpc-attribute-request)
  (match cmd
    [(op-create type cfg) (generic-create aws-vpc-model type cfg create-vpc
                                          #:api->mdl-fn (sub-result 'vpc)
                                          #:update-hdlr aws-vpc-hdlr)]
    [(op-read _ cfg) (generic-read vpc cfg)]
    [(op-update type cfg diff)
     (generic-update type cfg diff modify-vpc-attribute (diff->generic-update 'vpc-id diff)
                     aws-vpc-hdlr) ]
    [(op-delete _ cfg) (delete-vpc cfg)]
    [(op-diff _ oldcfg newcfg) (generic-diff aws-vpc-model oldcfg newcfg)]
    [_ (log-marv-warn (~a "ignoring unknown command:" cmd))]))

(define/res (aws-igw cmd) (create-internet-gateway-request internet-gateway #f)
  (match cmd
    [(op-create _ cfg) (hash-ref (create-internet-gateway cfg) 'internet-gateway)]
    [(op-read _ cfg) cfg]
    [(op-delete _ cfg) (delete-internet-gateway cfg)]
    [_ (log-marv-warn (~a "ignoring unknown command:" cmd))]))

(define/res (aws-igw-attachment cmd) (attach-internet-gateway-request attach-internet-gateway-request #f)
  (match cmd
    [(op-create _ cfg) (attach-internet-gateway cfg) cfg]
    [(op-read _ cfg) cfg]
    [(op-delete _ cfg) (detach-internet-gateway cfg)]
    [(op-diff _ oldcfg newcfg) (generic-diff aws-igw-attachment-model oldcfg newcfg)]
    [_ (log-marv-warn (~a "ignoring unknown command:" cmd))]))

(define/res (aws-elastic-ip cmd) (allocate-address-request allocate-address-result #f)
  (match cmd
    [(op-create _ cfg) (allocate-address cfg)]
    [(op-read _ cfg) cfg]
    [(op-delete _ cfg) (release-address (hash-remove cfg 'public-ip))]
    [_ (log-marv-warn (~a "ignoring unknown command:" cmd))]))

(define/res (aws-address-association cmd) (associate-address-request associate-address-result #f)
  (match cmd
    [(op-create _ cfg) (hash-merge< (associate-address cfg) cfg)]
    [(op-read _ cfg) cfg]
    [(op-delete _ cfg) (disassociate-address cfg)]
    [_ (log-marv-warn (~a "ignoring unknown command:" cmd))]))

(define instance-model (model-attributes-mutability 'run-instances-request 'instance
                                                    'modify-instance-attribute-request))

(define/res (aws-instance cmd) (run-instances-request instance #f)

  ;; the run-instances api call will return a reservation, with a list of instances inside, we only
  ;; need the first one.

  (define (api-security-groups->mdl cfg)
    (define sgs (hash-ref cfg 'security-groups)) ;; <- is a list of #hash(group-id,group-name)
    (define (ex-group-id h) (hash-ref h 'group-id))
    (hash-set cfg 'security-groups (map ex-group-id sgs)))

  (define (filtered-instance-req cfg)
    (hash 'instance-ids (list (hash-ref cfg 'instance-id))))

  (match cmd
    [(op-create type cfg)
     (generic-create instance-model type cfg run-instances
                     #:mdl->api-fn (add-cfg* 'max-count 1 'min-count 1)
                     #:api->mdl-fn (compose1 (rename-key 'security-groups 'security-group-ids)
                                             api-security-groups->mdl car (sub-result 'instances))) ]
    [(op-read _ cfg)
     (generic-readx cfg describe-instances
                    #:mdl->api-fn filtered-instance-req
                    #:api->mdl-fn (compose1 car (sub-result 'instances) car (sub-result 'reservations)))]
    [(op-delete _ cfg) (terminate-instances
                        (hash 'instance-ids (list (hash-ref cfg 'instance-id))))]
    [_ (log-marv-warn (~a "ignoring unknown command:" cmd))]))

(define/res (aws-subnet cmd) (create-subnet-request subnet modify-subnet-attribute-request)
  (match cmd
    [(op-create type cfg)
     (generic-create aws-subnet-model type cfg create-subnet
                     #:api->mdl-fn (sub-result 'subnet)
                     #:update-hdlr aws-subnet-hdlr)]
    [(op-read _ cfg) (generic-read subnet cfg)]
    [(op-update type cfg diff)
     (generic-update type cfg diff modify-subnet-attribute (diff->generic-update 'subnet-id diff) aws-subnet-hdlr) ]
    [(op-delete _ cfg) (delete-subnet cfg)]
    [(op-diff _ oldcfg newcfg) (generic-diff aws-subnet-model oldcfg newcfg)]
    [_ (log-marv-warn (~a "ignoring unknown command:" cmd))]))

(define/res (aws-security-group cmd) (create-security-group-request create-security-group-result #f)
  (match cmd
    [(op-create type cfg)
     (generic-create aws-security-group-model type cfg create-security-group
                     #:api->mdl-fn (merge-exclusive cfg))]
    [(op-read _ cfg) (generic-read security-group group-id cfg)]
    [(op-delete _ cfg) (delete-security-group cfg)]
    [(op-diff _ oldcfg newcfg) (generic-diff aws-security-group-model oldcfg newcfg)]
    [_ (log-marv-warn (~a "ignoring unknown command:" cmd))]))

;; ingress/egress-rule API allows us to create only one ingress/egress rule per invocation; and the
;; results are returned as lists with only one item in them.

(define security-group-ingress-rule-model (model-attributes-mutability
                                           'authorize-security-group-ingress-request
                                           'security-group-rule))

(define/res (aws-security-group-ingress-rule cmd)
  (security-group-rule security-group-rule #f)
  (match cmd
    [(op-create type cfg)
     (generic-create security-group-ingress-rule-model type cfg authorize-security-group-ingress
                     #:mdl->api-fn (rename-key 'cidr-ipv4 'cidr-ip)
                     #:api->mdl-fn (compose1 car (sub-result 'security-group-rules)))]
    [(op-read _ cfg) (generic-read security-group-rule cfg)]
    ;; [(op-update _ cfg _) void]
    [(op-delete _ cfg)
     (revoke-security-group-ingress
      (hash 'group-id (hash-ref cfg 'group-id)
            'security-group-rule-ids (list (hash-ref cfg 'security-group-rule-id))))]
    [(op-diff _ oldcfg newcfg)
     (generic-diff security-group-ingress-rule-model oldcfg newcfg)]
    [_ (log-marv-warn (~a "ignoring unknown command:" cmd))]))

;; TODO: can we use lack of 'output' shape in operation to 'detect' requestId/return format?

;; TODO: aws-network-acl-model has 'entries', but we remove those during create, so how do we keep the
;; two aspects in sync? Way to remove from model?
;; e.g. (define network-acl-model (hash-remove aws-network-acl-model 'entries))
;; does not work so some model shaping tools would be useful.

(define/res (aws-network-acl cmd) (create-network-acl-request network-acl #f)
  (match cmd
    [(op-create type cfg)
     (generic-create aws-network-acl-model type cfg create-network-acl
                     #:api->mdl-fn (compose1 (remove-cfg 'entries) (sub-result 'network-acl)))]
    [(op-read _ cfg) (generic-read network-acl cfg)]
    [(op-diff _ oldcfg newcfg) (generic-diff aws-network-acl-model oldcfg newcfg)]
    [(op-delete _ cfg) (delete-network-acl cfg)]
    [_ (log-marv-warn (~a "ignoring unknown command:" cmd))
       cmd]))

(define/res (aws-network-acl-entry cmd) (create-network-acl-entry-request network-acl-entry #f)
  (match cmd
    [(op-create type cfg)
     (create-network-acl-entry cfg)
     cfg]
    [(op-read _ cfg) cfg]
    [(op-diff _ oldcfg newcfg) (generic-diff aws-network-acl-model oldcfg newcfg)]
    [(op-delete _ cfg) (delete-network-acl-entry cfg)]
    [_ (log-marv-warn (~a "ignoring unknown command:" cmd))
       cmd]))

;; Useful techniques when adding a new resource:
;; (pretty-display (model-attrs-all aws-network-acl-model))
;; (pretty-display (model-attrs-all aws-network-acl-entry-model))
;; (syntax->datum (expand #'(generic-read network-acl (hash))))
