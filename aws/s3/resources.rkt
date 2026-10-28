#lang racket

(provide
 types-lookup
 aws-s3-bucket)

(require
  marv/model
  marv/aws/types
  marv/utils
  marv/resources
  marv/logger
  marv/aws/s3/ops
  marv/aws/s3/shapes
  (only-in marv/state state-ref state-in?))

;; TODO: this is common

(define (mk-comp-resource type id drvr cfg [check? #f])

  ;; Creates a new resource, compositing the shape's blueprint with the given configuration. Returns a
  ;; computation struct containing the merged config and computation-shape

  ;; TODO: convert to using a model as input, once all resource models are ready

  ;; FIXME: !! the state-ref in here is a smell. There may be a better method to pass in existing
  ;; configuration from current state, if it exists, rather than setting up all the computations

  (define (safe-state-ref) (if (state-in? id) (computation-hs (resource-cfg (state-ref id))) 'undef))

  (define model
    (case type
      [(aws-s3-bucket) bucket-model]
      [else (error 'docker "unsupported s3 resource:~a" type)]))

  (resource
   type id
   (driver-id drvr)
   (computation
    (hash-merge<
     cfg
     ((mk-blueprint-fn model) safe-state-ref)))))

(define (((aws-s3-bucket cfg)drvr)id)
  (mk-comp-resource 'aws-s3-bucket id drvr cfg))

(define bucket-model
  #hash((name . immutable)
        (region . immutable)))

(define/hdlr (s3-bucket cmd)
  (log-marv-debug (~a "aws-s3-bucket " cmd))
  (match cmd
    [(op-create type cfg)
     (define req (create-bucket-request
                  #:bucket  (hash-ref cfg 'name)
                  #:create-bucket-configuration
                  (create-bucket-configuration #:location-constraint (hash-ref cfg 'region))))
     (create-bucket req)
     cfg
     ]
    [(op-read _ cfg) cfg]
    [(op-diff _ oldcfg newcfg) (generic-diff (hash->model-attrs bucket-model) oldcfg newcfg)]
    [(op-delete _ cfg)
     (delete-bucket (delete-bucket-request #:bucket (hash-ref cfg 'name)))]
    [_ (log-marv-debug (~a "ignoring unknown command:" cmd))
       cmd]))

