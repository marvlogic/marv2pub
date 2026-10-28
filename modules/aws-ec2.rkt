#lang racket

(provide main)

(require
  marv/aws/driver
  marv/marvd
  marv/aws/ec2/resources
  marv/aws/barrier ;; remove to test the (interactive) resolver
  marv/stx-utils
  marv/state
  )

(displayln "")
(displayln "********************************************************************************")

(define MY-IP "0.0.0.0/32")

(define (networking marvd aws)

  (:= vpc (:: aws aws-vpc
              (<= cidr-block "10.0.0.0/16"
                  enable-dns-support #t)))

  (::= vid marvd svar (rref vpc vpc-id))

  (::= igw aws aws-igw (mkih))

  (::= igwatt aws aws-igw-attachment
       (<= vpc-id (vid)
           internet-gateway-id (rref igw internet-gateway-id)))

  (::= sn1 aws aws-subnet
       (<= vpc-id (rref vpc vpc-id)
           map-public-ip-on-launch #t
           cidr-block "10.0.2.0/24"))

  (::= sn2 aws aws-subnet
       (<= vpc-id (rref vpc vpc-id)
           map-public-ip-on-launch #t
           cidr-block "10.0.3.0/24"))

  (::= _internal marvd checkpoint (vpc) (igw) (sn1) (sn2))

  (:: marvd checkpoint
      (<= vpc-id (rref vpc vpc-id)
          sn1 (rref sn1 subnet-id)
          sn2 (rref sn2 subnet-id)
          igwatt (rref igwatt internet-gateway-id))))

(define (mk-instance marvd aws sn-id sg-id)
  (::= key aws aws-key-pair (<= key-name "marv2"))
  (::= address-allocation aws aws-elastic-ip (mkih))

  (::= instance aws aws-instance
       (<=
         image-id "ami-015b1e8e2a6899bdb"
         instance-type "t2.micro"
         key-name (rref key key-name)
         security-group-ids (list sg-id)
         subnet-id sn-id))

  (::= assocn aws aws-address-association
       (<=
         allocation-id (rref address-allocation allocation-id)
         instance-id (rref instance instance-id)))

  (::= _internal marvd checkpoint (key) (address-allocation) (instance) (assocn))

  (:: marvd checkpoint
      (<= instance (rref instance instance-id))))

(define (main [purge? #f])
  (:= marvd marv-driver)
  (:= aws aws-driver)

  (when (not purge?)

    (:= network (networking marvd aws))

    ;; (::= ip-address marvd svar (rref address-allocation public-ip))

    (::= sg aws aws-security-group
         (<= vpc-id (rref network vpc-id)
             group-name "marv-web-sg"
             description "Marv Sec group"
             ))

    (::= ing aws aws-security-group-ingress-rule
         (<=
           group-id (rref sg group-id)
           ip-protocol "-1"
           cidr-ipv4 MY-IP))

    ;; TODO: 2026 marv will attempt to remove the security group before the instance is deleted.
    ;;Because of modules problem.

    (:= instance (mk-instance marvd aws (rref network sn1) (rref sg group-id)))
    #t)
  )

