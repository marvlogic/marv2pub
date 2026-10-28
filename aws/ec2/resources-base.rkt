#lang racket

(provide
 define/res
 )
(require
  marv/resource-stx
  racket/hash
  marv/utils
  marv/resources
  marv/logger
  marv/aws/types
  marv/aws/ec2/boto
  (only-in marv/state state-ref state-in?))

;; TODO: id parameters seem to be unused - and yet; the driver could support forms of replacement
;; references e.g. "$id". This may allow e.g. tags to contain references to the controlling marv
;; resource

(define (check-io-blueprint in-shp mod-shp out-shp cfg check-merge?)

  ;; Checks the default assumption that we can merge together in, mod and result (out) shapes of the
  ;; AWS api shapes. This assumption is the basis for most of the model specifications.

  (define (merge-blueprints b1 b2)
    ;; merge the two given blueprint shapes into a single blueprint this can be
    ;; useful when a unified i/o shape is required to be a configuration format
    ;; for a resource

    (define (combine a1 a2)
      (if (equal? a1 a2) a1
          (error "types are not compatible during merge"  a1 a2 )))

    (define (merge s1 s2) (hash-union s1 s2 #:combine combine))
    (merge b1 b2))

  (define inbp (mk-blueprint in-shp))
  (define modbp (if mod-shp (mk-blueprint mod-shp) (hash)))
  (define keys (hash-keys (hash-union inbp modbp)))
  (unless
      (or (subset? (hash-keys cfg) keys))
    (raise-argument-error in-shp (~a "attributes" keys)
                          (set-subtract (hash-keys cfg) keys)))
  (when check-merge?
    (log-marv-debug (~a "checking for compatible merge:" in-shp out-shp))
    (merge-blueprints (mk-blueprint in-shp) (mk-blueprint out-shp))))


;; TODO: This function is similar across s3, docker.

(define (mk-comp-resource type id drvr request-shp-id result-shp-id cfg [check? #t]
                          [mod-request-shp-id #f])

  ;; Creates a new resource, compositing the shape's blueprint with the given configuration. Returns a
  ;; computation struct containing the merged config and computation-shape
  ;;
  ;; - type should be the same as the AWS resource function name (TODO:)
  ;;
  ;; - drvr will be passed through from the resource function
  ;;
  ;; - request-shp-id is the base shape of the resource
  ;;
  ;; - result-shp-id is the result from  calling the API to create the resource
  ;;
  ;; - cfg is the object definition when it's being created (a hash)
  ;;
  ;; - check? pass #t (default) to check that the merging of the shapes is valid
  ;;
  ;; - mod-request-shp-id a shape-id of a modification request; all the attributes of the given shape
  ;; are marked as mutable within the model.

  ;; TODO: convert to using a model as input, once all resource models are ready

  (check-io-blueprint request-shp-id mod-request-shp-id result-shp-id cfg check?)

  ;; FIXME: !! the state-ref in here is a smell. There may be a better method to pass in existing
  ;; configuration from current state, if it exists, rather than setting up all the computations
  ;;

  (define (safe-state-ref) (if (state-in? id) (computation-hs (resource-cfg (state-ref id))) 'undef))
  (resource
   type id
   (driver-id drvr)
   (computation
    (hash-merge<
     cfg
     (mk-computation-shape result-shp-id safe-state-ref)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Implementing support for a new ec2 resource
;;

;; Define a model for the resource; this is blueprint for the structure that a user defines when they
;; want to create a resource of that type. 'model-attributes-mutability' is a helper function that
;; scans the boto specification for the given shape ids, and comes out with a sensible default model;
;; assuming it's OK to have something close to the AWS APIs. The helper can define the model from the
;; create request, and (optionally) the read and modification request(s).

;; Define the resource's function. The order and nesting of this function's parameters is crucial to
;; marv's resource handling; marv's resource scheduler will use it to construct resources of this type
;; in the resource graph. This is the function that gets called when you do:
;;
;;    key := aws :: aws-key-pair { config ... }
;;
;; ASIDE: marv's syntax handling re-arranges the above statement into this racket:
;;
;;    (define key (((aws-key-pair {config...}) aws) 'key) )
;;
;; Often the function just calls mk-comp-resource to handle this, assuming that you want the model to
;; be very similar to the actual AWS API objects. See mk-comp-resource above for parameter
;; information.

;; Define the handler function which is called by the AWS driver. This function handles the 'op-*'
;; command values passed to it from the 'cmd' parameter. Commands are sent during the various stages
;; of a resource's life cycle. 'define/hdlr' will automatically wire the function into the type
;; handling for the driver; in this case, the function name is 'key-pair' so the driver will be wired
;; to send requests for resource type 'aws-key-pair' to this function.
;;
;; Sometimes it's necessary to transform the request or responses to/from the API. See vpc below for
;; an example of how/why this is done.


(define-syntax (define/res stx)
  (define/prefix-resource #'types-lookup #'mk-comp-resource #'model-attributes-mutability stx))

