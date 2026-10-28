#lang racket

(require
  rackunit
  rackunit/text-ui
  marv/aws/boto-utils)

(require/expose marv/aws/boto (mk-headers))

(define boto-ec2 (with-input-from-file "aws/ec2/service-2.json" (lambda () (read-json-string-keys))))

(exit
  (run-tests
    (test-suite
      "API tests"
      (test-case
        "EC2 headers"
        (check-equal?
          (mk-headers (hash "abc" 123 "def" (list (hash "name" "Kevin" "values" `(1 2 3))) "z" (hash "a" 1 "b" 2)))
          '((Z.A . "1")
            (Z.B . "2")
            (Abc . "123")
            (Def.0.Name . "Kevin")
            (Def.0.Values.2 . "3")
            (Def.0.Values.1 . "2")
            (Def.0.Values.0 . "1")))

        (check-equal?
          (mk-headers #hash(("Filter" . ((#hash(("Name" . "cidr") ("Value" . ("10.10" "10.45"))))))))
          '((Filter.0.0.Value.1 . "10.45")
            (Filter.0.0.Value.0 . "10.10")
            (Filter.0.0.Name . "cidr")))


        )

      (test-case
        "computation-tests"
        (define (subnet-root)
          (hash 'subnet
                (hash 'subnet-id "sn123")))

        (check-equal?
          ((hash-ref (hash-ref (mk-computation-shape boto-ec2 'create-subnet-result subnet-root) 'subnet) 'subnet-id))
          "sn123")

        (define (vpc-root)
          (hash 'next-token "123"
                'vpcs (list (hash 'vpc-id "vpc-123"))))

        (check-equal?
          ((hash-ref (mk-computation-shape boto-ec2 'describe-vpcs-result vpc-root) 'next-token))
          "123")

        (check-equal?
          ((hash-ref (car (hash-ref (mk-computation-shape boto-ec2 'describe-vpcs-result vpc-root) 'vpcs)) 'vpc-id))
          "vpc-123")
        )

      (test-case
        "subnet-test"
        (define subnet-test1
          '((requestId () 62ce2829-fd5c-4912-8c1a-fa4380163682)
            (subnet
              ()
              (subnetId () subnet-0db0ea01ad79e6e89)
              (subnetArn () arn:aws:ec2:eu-west-1:999999999999:subnet/subnet-0db0ea01ad79e6e89)
              (state () available)
              (ownerId () 999999999999)
              (vpcId () vpc-073323f0e7be774f4)
              (cidrBlock () 10.0.1.0/24)
              (ipv6CidrBlockAssociationSet ())
              (availableIpAddressCount () "251")
              (availabilityZone () eu-west-1c)
              (availabilityZoneId () euw1-az3)
              (defaultForAz () "true")
              (mapPublicIpOnLaunch () "false")
              (assignIpv6AddressOnCreation () "false")
              (privateDnsNameOptionsOnLaunch
                ()
                (hostnameType () ip-name)
                (enableResourceNameDnsARecord () "false")
                (enableResourceNameDnsAAAARecord () "false")
                )
              (ipv6Native () "false")
              (enableDns64 () "false")
              )))
        (check-equal?
          (parse-resp boto-ec2 "CreateSubnetResult" subnet-test1)
          #hash(
                (subnet . #hash(
                                (assign-ipv6address-on-creation . #f)
                                (availability-zone . "eu-west-1c")
                                (availability-zone-id . "euw1-az3")
                                (available-ip-address-count . 251)
                                (block-public-access-states . undef)
                                (cidr-block . "10.0.1.0/24")
                                (customer-owned-ipv4pool . undef)
                                (default-for-az . #t)
                                (enable-dns64 . #f)
                                (enable-lni-at-device-index . undef)
                                (ipv6cidr-block-association-set . undef)
                                (ipv6native . #f)
                                (map-customer-owned-ip-on-launch . undef)
                                (map-public-ip-on-launch . #f)
                                (outpost-arn . undef)
                                (owner-id . "999999999999")
                                (private-dns-name-options-on-launch . #hash((enable-resource-name-dns-aaaarecord . #f)
                                                                            (enable-resource-name-dns-arecord . #f)
                                                                            (hostname-type . "ip-name")))
                                (state . "available")
                                (subnet-arn . "arn:aws:ec2:eu-west-1:999999999999:subnet/subnet-0db0ea01ad79e6e89")
                                (subnet-id . "subnet-0db0ea01ad79e6e89")
                                (tags . undef)
                                (type . undef)
                                (vpc-id . "vpc-073323f0e7be774f4"))))))


      ;; (check-equal? (primitive-shape-of ( hash-ref (members-of 'AllocateAddressRequest) 'PublicIpv4Pool )) 'string)
      ;; (check-equal? (primitive-shape-of ( hash-ref (members-of 'AllocateAddressRequest) 'TagSpecifications )) 'list)
      ;; (check-equal? (primitive-shape-of ( hash-ref (members-of 'CreateFleetRequest) 'TargetCapacitySpecification )) 'structure)
      )))

