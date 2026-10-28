#lang racket

(require
  marv/aws/s3/ops
  marv/aws/s3/shapes
  marv/logger)

(displayln "")
(displayln "********************************************************************************")

(log-level-set 'info)
;; (log-filter-set ".*boto.*.rkt")
(log-marv-info "** HELLO FROM MARV **")

(define list-req
  (list-buckets-request #:max-buckets 1))

(displayln list-req)
(define buckets (list-buckets list-req))
(pretty-display buckets)
(define mb-req
  (create-bucket-request
   #:bucket "happyrat-2212"
   #:create-bucket-configuration
   (create-bucket-configuration
    #:location-constraint
    "eu-west-1")))

(define resp (create-bucket mb-req ))
(pretty-display resp)

(define rbreq
  (delete-bucket-request
   #:bucket "happyrat-2212"))

(define respd (delete-bucket rbreq))
(pretty-display respd)


