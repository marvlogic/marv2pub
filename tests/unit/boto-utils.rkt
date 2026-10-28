#lang racket

(require
  marv/model
  marv/aws/boto-utils
  xml
  rackunit
  rackunit/text-ui
  marv/logger
  )

(define boto-s3 (with-input-from-file "aws/s3/service-2.json" (lambda () (read-json-string-keys))))
(define boto-ec2 (with-input-from-file "aws/ec2/service-2.json" (lambda () (read-json-string-keys))))

(log-level-set 'info)
(exit
  (run-tests
    (test-suite
      "boto-utils"

      (test-case
        "mk-request-parts for CreateBucketRequest"
        (check-equal?
          (mk-request-parts
            boto-s3
            #hasheq((bucket . "bucket123")
                    (acl . "canned-acl")
                    (create-bucket-configuration . #hasheq((location-constraint . "eu-west-1"))))
            "CreateBucketRequest")
          (request-part
            (make-hash '(("x-amz-acl" . "canned-acl")))
            (make-hash '(("Bucket" . "bucket123")))
            (make-hash)
            (make-hash
              (list (cons "CreateBucketConfiguration" (make-hash '(("LocationConstraint" . "eu-west-1")))))))))

      #; (test-case
           "mk-request-parts for PutObjectRequest/metadata - ***FAILS***"
           (check-equal?
             (mk-request-parts
               boto-s3
               #hasheq((bucket . "bucket123")
                       (key . "object-key")
                       (metadata . #hasheq((meta1 . "eu-west-1"))))
               "PutObjectRequest")
             (request-part
               (make-hash '(("x-amz-meta-meta1" . "eu-west-1")))
               (make-hash '(("Bucket" . "bucket123")))
               (make-hash)
               (make-hash))))

      (test-case
        "mk-request-parts for Instance"
        (check-equal?
          (mk-request-parts
            boto-ec2
            #hasheq((min-count . 1) (max-count . 1) (image-id . "ami-1234"))
            "RunInstancesRequest")
          (request-part
            (make-hash)
            (make-hash)
            (make-hash)
            (make-hash '(("ImageId" . "ami-1234") ("MaxCount" . 1) ("MinCount" . 1))))))

      (test-case
        "Create bucket request"
        (define cbr (hash "CreateBucketConfiguration" (hash "LocationConstraint" "eu-west-1")))
        (define serialised-cbr (serialise-shape boto-s3 "CreateBucketRequest" cbr))
        (define expect-cbr
          '(CreateBucketRequest
             ()
             (CreateBucketConfiguration ((xmlns "http://s3.amazonaws.com/doc/2006-03-01/"))
                                        (LocationConstraint () "eu-west-1"))))

        (check-equal? serialised-cbr expect-cbr)
        (check-equal? (xexpr->string serialised-cbr) (xexpr->string expect-cbr))
        (check-equal? (serialise-payload boto-s3 "CreateBucketRequest" cbr)
                      '(CreateBucketConfiguration ((xmlns "http://s3.amazonaws.com/doc/2006-03-01/"))
                                                  (LocationConstraint () "eu-west-1")))
        (check-equal? (serialise-shape boto-s3 "CreateBucketRequest" (hash)) '(CreateBucketRequest ())))

      (test-case
        "Target grant"
        (define expect-target-grant '(TargetGrant () (Grantee ((xsi:type "CanonicalUser")))))
        (define serialised-tg (serialise-shape boto-s3 "TargetGrant" (hash "Grantee" (hash "Type" "CanonicalUser"))))
        (check-equal? serialised-tg expect-target-grant)
        (check-equal? (xexpr->string serialised-tg) (xexpr->string expect-target-grant)))

      (test-case
        "EC2 shape"
        (define model-attrs
          (model-attrs-all (model-attributes-mutability boto-ec2 'create-subnet-request 'subnet
                                                        'modify-subnet-attribute-request)))
        (check-equal? (hash-ref model-attrs 'subnet-id) 'read-only)
        (check-equal? (hash-ref model-attrs 'availability-zone) 'immutable))


      (test-case
        "Parse S3 CreateBucketOutput response"
        (define resp-headers
          #hash(
                (date . "Mon, 20 Oct 2025 08:26:22 GMT")
                (location . ("http://happyrat-2212.s3.amazonaws.com/"))
                (content-Length . 0)
                (x-amz-bucket-arn . ("bucketarn" . "second"))
                (server . "AmazonS3")))

        (check-equal?
          (parse-response-parts boto-s3 "CreateBucketOutput"
                                (lambda (h) (car (hash-ref resp-headers h)))
                                (lambda () "shouldn't be called"))
          #hash((bucket-arn . "bucketarn")
                (location . "http://happyrat-2212.s3.amazonaws.com/"))))

      (test-case
        "Parse S3 DeleteObjectsOutput response"
        (define resp-headers
          #hash(
                (date . "Mon, 20 Oct 2025 08:26:22 GMT")
                (x-amz-request-charged . (100))
                (location . ("http://happyrat-2212.s3.amazonaws.com/"))
                (content-Length . 0)
                (server . "AmazonS3")))
        (define resp-body
          '((Deleted () (DeletedObject () (Key () "key") (VersionId () "123")))
            (Error () (Error () (Key () "key") (VersionId () "123")))
            ))

        (check-equal?
          (parse-response-parts boto-s3 "DeleteObjectsOutput"
                                (lambda (h) (car (hash-ref resp-headers h)))
                                (lambda () resp-body))
          #hash(
                (deleted . (#hash(
                                  (delete-marker . undef)
                                  (delete-marker-version-id . undef)
                                  (key . "key")
                                  (version-id . "123"))))
                (errors . (#hash(
                                 (code . undef)
                                 (key . "key")
                                 (message . undef)
                                 (version-id . "123"))))
                (request-charged . 100)))
        )

      )))
