#lang racket

(provide
 vpc-instances-state
 )

(require
  marv/aws/ec2/ops
  marv/aws/ec2/shapes
  )

(define (vpc-instances-state vpc-id)
  (define resp
    (describe-instances
     (describe-instances-request
      #:filters
      (list (filter #:name "vpc-id" #:values (list vpc-id))
            (filter #:name "instance-state-name" #:values (list "pending" "running" "shutting-down" "stopping" "stopped"))
            ))))
  (if (eq? 'undef (hash-ref resp 'reservations)) '()
      (map (lambda(i)(cons (hash-ref i 'instance-id) (hash-ref i 'state)))
           (hash-ref (car (hash-ref resp 'reservations)) 'instances))))

