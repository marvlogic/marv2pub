#lang racket

(provide main)

(require
  marv/aws/driver
  marv/aws/ec2/resources
  marv/aws/barrier        ;; remove to test the (interactive) resolver
  marv/stx-utils
  marv/state
  marv/svar
  )

(displayln "")
(displayln "********************************************************************************")

(define (main [purge? #f])
  (:= svard svar-driver)
  (:= aws aws-driver)

  (when (not purge?)
    (:> vpc := aws :: aws-vpc cidr-block "10.0.0.0/16")

    (::= vid svard svar (rref vpc vpc-id))

    (::= igw aws aws-igw (mkih))

    (::= igwatt aws aws-igw-attachment (mkih vpc-id (vid)
                                             internet-gateway-id (rref igw internet-gateway-id)))

    (::= sn1 aws aws-subnet (mkih vpc-id (vid)
                                  map-public-ip-on-launch #t
                                  cidr-block "10.0.2.0/24"))

    (::= acl aws aws-network-acl (mkih vpc-id (vid)))
    (::= entry aws aws-network-acl-entry
         (mkih network-acl-id (rref acl network-acl-id)
               protocol "17"
               cidr-block (rref vpc cidr-block)
               port-range (mkih from 53 to 53)
               egress #f
               rule-action "allow"
               rule-number 110
               ))

    #t)
  )

