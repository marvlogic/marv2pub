#lang racket
(require
  marv/resources
  marv/aws/ec2/ops
  marv/logger)

(provide ec2driver)

(define (sym->proc sym)
  (hash-ref
   (hash
    'create-subnet create-subnet
    'delete-subnet delete-subnet
    'describe-vpcs describe-vpcs
    'describe-internet-gateways describe-internet-gateways
    'create-key-pair create-key-pair
    'delete-key-pair delete-key-pair
    'create-security-group create-security-group
    'delete-security-group delete-security-group
    )
   sym))

(define aws-exec-driver
  (mk-res-driver
   'execdrv
   ; This is a simple execution-only driver, so only accept 'create commands
   (lambda(lc-cmd)
     (log-marv-debug lc-cmd)
     (match lc-cmd
       [(op-create type cfg)
        ; conversion to wire protocol happens from here onwards
        (log-marv-debug (~a "exec handler " type ))
        (log-marv-debug (~a "exec config " cfg))
        ((sym->proc type) cfg)]
       [_ (log-marv-debug (~a "Ignoring " lc-cmd))]
       ))))

(define ec2driver
  (lambda(id)
    (driver (res-driver-id aws-exec-driver) id
            (res-driver-exec aws-exec-driver) null)))

