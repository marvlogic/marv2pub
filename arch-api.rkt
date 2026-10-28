#lang racket

(require
  marv/aws/ec2/ops
  marv/aws/ec2/shapes
  "logger.rkt")

(displayln "")
(displayln "********************************************************************************")

(log-level-set 'debug)
(log-filter-set "(aws-api-drvr|aws-resources|arch-api|state|tasks|.*barrier.*).rkt")
(log-marv-info "** HELLO FROM MARV **")
(log-marv-info "STATE-INITIAL")

(define vpc-id "vpc-0313dfb929c6ad0dd")
(define vpc-req (describe-vpcs-request))
(pretty-display (describe-vpcs vpc-req))

(define gateways-req
  (describe-internet-gateways-request #:filters (list (filter #:name
                                                              "attachment.vpc-id"
                                                              #:values (list vpc-id)))))
(displayln gateways-req)
(define gateways (describe-internet-gateways gateways-req))
(displayln gateways)
(pretty-display (hash-ref gateways 'internet-gateways) )
(define gwid (hash-ref (car (hash-ref gateways 'internet-gateways)) 'internet-gateway-id))
(displayln gwid)
;; (detach-internet-gateway (hash 'vpc-id  vpc-id'internet-gateway-id gwid ))

;; (vpc-instances-state vpc-id)
;;(:= delete (:: ec2drv (ec2-api detach internet-gateway) (hash 'dry-run #t)))
;;(plan-apply)
