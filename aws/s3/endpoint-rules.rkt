#lang racket
(require marv/endpoint-rules)
(provide s3-endpoint-rules)
(define (log x)
  (displayln x)
  #t)
(define (s3-endpoint-rules endpoint)
  (rules
   (conditions
    (isSet (ref "Region"))
    (rules
     (conditions (booleanEquals (ref "Accelerate") #t)
                 (booleanEquals (ref "UseFIPS") #t)
                 (error "Accelerate cannot be used with FIPS"))
     (conditions (booleanEquals (ref "UseDualStack") #t)
                 (isSet (ref "Endpoint"))
                 (error "Cannot set dual-stack in combination with a custom endpoint."))
     (conditions (isSet (ref "Endpoint"))
                 (booleanEquals (ref "UseFIPS") #t)
                 (error "A custom endpoint cannot be combined with FIPS"))
     (conditions (isSet (ref "Endpoint"))
                 (booleanEquals (ref "Accelerate") #t)
                 (error "A custom endpoint cannot be combined with S3 Accelerate"))
     (conditions (booleanEquals (ref "UseFIPS") #t)
                 (assign "partitionResult" (aws.partition (ref "Region")))
                 (stringEquals (getAttr (ref "partitionResult") "name") "aws-cn")
                 (error "Partition does not support FIPS"))
     (conditions
      (isSet (ref "Bucket"))
      (assign "bucketSuffix" (substring (ref "Bucket") 0 6 #t))
      (stringEquals (ref "bucketSuffix") "--x-s3")
      (rules
       (conditions (booleanEquals (ref "UseDualStack") #t)
                   (error "S3Express does not support Dual-stack."))
       (conditions (booleanEquals (ref "Accelerate") #t)
                   (error "S3Express does not support S3 Accelerate."))
       (conditions
        (isSet (ref "Endpoint"))
        (assign "url" (parseURL (ref "Endpoint")))
        (rules
         (conditions
          (isSet (ref "DisableS3ExpressSessionAuth"))
          (booleanEquals (ref "DisableS3ExpressSessionAuth") #t)
          (rules
           (conditions
            (booleanEquals (getAttr (ref "url") "isIp") #t)
            (rules (conditions
                    (assign "uri_encoded_bucket" (uriEncode (ref "Bucket")))
                    (rules (conditions
                            (endpoint
                             "{url#scheme}://{url#authority}/{uri_encoded_bucket}{url#path}"))))))
           (conditions
            (aws.isVirtualHostableS3Bucket (ref "Bucket") #f)
            (rules (conditions (endpoint "{url#scheme}://{Bucket}.{url#authority}{url#path}"))))
           (conditions (error "S3Express bucket name is not a valid virtual hostable name."))))
         (conditions
          (booleanEquals (getAttr (ref "url") "isIp") #t)
          (rules (conditions
                  (assign "uri_encoded_bucket" (uriEncode (ref "Bucket")))
                  (rules (conditions
                          (endpoint
                           "{url#scheme}://{url#authority}/{uri_encoded_bucket}{url#path}"))))))
         (conditions
          (aws.isVirtualHostableS3Bucket (ref "Bucket") #f)
          (rules (conditions (endpoint "{url#scheme}://{Bucket}.{url#authority}{url#path}"))))
         (conditions (error "S3Express bucket name is not a valid virtual hostable name."))))
       (conditions
        (isSet (ref "UseS3ExpressControlEndpoint"))
        (booleanEquals (ref "UseS3ExpressControlEndpoint") #t)
        (rules
         (conditions
          (assign "partitionResult" (aws.partition (ref "Region")))
          (rules
           (conditions
            (assign "uri_encoded_bucket" (uriEncode (ref "Bucket")))
            (not (isSet (ref "Endpoint")))
            (rules
             (conditions
              (booleanEquals (ref "UseFIPS") #t)
              (endpoint
               "https://s3express-control-fips.{Region}.{partitionResult#dnsSuffix}/{uri_encoded_bucket}"))
             (conditions
              (endpoint
               "https://s3express-control.{Region}.{partitionResult#dnsSuffix}/{uri_encoded_bucket}"))))))))
       (conditions
        (aws.isVirtualHostableS3Bucket (ref "Bucket") #f)
        (rules
         (conditions
          (assign "partitionResult" (aws.partition (ref "Region")))
          (rules
           (conditions
            (isSet (ref "DisableS3ExpressSessionAuth"))
            (booleanEquals (ref "DisableS3ExpressSessionAuth") #t)
            (rules
             (conditions
              (assign "s3expressAvailabilityZoneId" (substring (ref "Bucket") 6 14 #t))
              (assign "s3expressAvailabilityZoneDelim" (substring (ref "Bucket") 14 16 #t))
              (stringEquals (ref "s3expressAvailabilityZoneDelim") "--")
              (rules
               (conditions
                (booleanEquals (ref "UseFIPS") #t)
                (endpoint
                 "https://{Bucket}.s3express-fips-{s3expressAvailabilityZoneId}.{Region}.{partitionResult#dnsSuffix}"))
               (conditions
                (endpoint
                 "https://{Bucket}.s3express-{s3expressAvailabilityZoneId}.{Region}.{partitionResult#dnsSuffix}"))))
             (conditions
              (assign "s3expressAvailabilityZoneId" (substring (ref "Bucket") 6 15 #t))
              (assign "s3expressAvailabilityZoneDelim" (substring (ref "Bucket") 15 17 #t))
              (stringEquals (ref "s3expressAvailabilityZoneDelim") "--")
              (rules
               (conditions
                (booleanEquals (ref "UseFIPS") #t)
                (endpoint
                 "https://{Bucket}.s3express-fips-{s3expressAvailabilityZoneId}.{Region}.{partitionResult#dnsSuffix}"))
               (conditions
                (endpoint
                 "https://{Bucket}.s3express-{s3expressAvailabilityZoneId}.{Region}.{partitionResult#dnsSuffix}"))))
             (conditions
              (assign "s3expressAvailabilityZoneId" (substring (ref "Bucket") 6 19 #t))
              (assign "s3expressAvailabilityZoneDelim" (substring (ref "Bucket") 19 21 #t))
              (stringEquals (ref "s3expressAvailabilityZoneDelim") "--")
              (rules
               (conditions
                (booleanEquals (ref "UseFIPS") #t)
                (endpoint
                 "https://{Bucket}.s3express-fips-{s3expressAvailabilityZoneId}.{Region}.{partitionResult#dnsSuffix}"))
               (conditions
                (endpoint
                 "https://{Bucket}.s3express-{s3expressAvailabilityZoneId}.{Region}.{partitionResult#dnsSuffix}"))))
             (conditions
              (assign "s3expressAvailabilityZoneId" (substring (ref "Bucket") 6 20 #t))
              (assign "s3expressAvailabilityZoneDelim" (substring (ref "Bucket") 20 22 #t))
              (stringEquals (ref "s3expressAvailabilityZoneDelim") "--")
              (rules
               (conditions
                (booleanEquals (ref "UseFIPS") #t)
                (endpoint
                 "https://{Bucket}.s3express-fips-{s3expressAvailabilityZoneId}.{Region}.{partitionResult#dnsSuffix}"))
               (conditions
                (endpoint
                 "https://{Bucket}.s3express-{s3expressAvailabilityZoneId}.{Region}.{partitionResult#dnsSuffix}"))))
             (conditions
              (assign "s3expressAvailabilityZoneId" (substring (ref "Bucket") 6 26 #t))
              (assign "s3expressAvailabilityZoneDelim" (substring (ref "Bucket") 26 28 #t))
              (stringEquals (ref "s3expressAvailabilityZoneDelim") "--")
              (rules
               (conditions
                (booleanEquals (ref "UseFIPS") #t)
                (endpoint
                 "https://{Bucket}.s3express-fips-{s3expressAvailabilityZoneId}.{Region}.{partitionResult#dnsSuffix}"))
               (conditions
                (endpoint
                 "https://{Bucket}.s3express-{s3expressAvailabilityZoneId}.{Region}.{partitionResult#dnsSuffix}"))))
             (conditions (error "Unrecognized S3Express bucket name format."))))
           (conditions
            (assign "s3expressAvailabilityZoneId" (substring (ref "Bucket") 6 14 #t))
            (assign "s3expressAvailabilityZoneDelim" (substring (ref "Bucket") 14 16 #t))
            (stringEquals (ref "s3expressAvailabilityZoneDelim") "--")
            (rules
             (conditions
              (booleanEquals (ref "UseFIPS") #t)
              (endpoint
               "https://{Bucket}.s3express-fips-{s3expressAvailabilityZoneId}.{Region}.{partitionResult#dnsSuffix}"))
             (conditions
              (endpoint
               "https://{Bucket}.s3express-{s3expressAvailabilityZoneId}.{Region}.{partitionResult#dnsSuffix}"))))
           (conditions
            (assign "s3expressAvailabilityZoneId" (substring (ref "Bucket") 6 15 #t))
            (assign "s3expressAvailabilityZoneDelim" (substring (ref "Bucket") 15 17 #t))
            (stringEquals (ref "s3expressAvailabilityZoneDelim") "--")
            (rules
             (conditions
              (booleanEquals (ref "UseFIPS") #t)
              (endpoint
               "https://{Bucket}.s3express-fips-{s3expressAvailabilityZoneId}.{Region}.{partitionResult#dnsSuffix}"))
             (conditions
              (endpoint
               "https://{Bucket}.s3express-{s3expressAvailabilityZoneId}.{Region}.{partitionResult#dnsSuffix}"))))
           (conditions
            (assign "s3expressAvailabilityZoneId" (substring (ref "Bucket") 6 19 #t))
            (assign "s3expressAvailabilityZoneDelim" (substring (ref "Bucket") 19 21 #t))
            (stringEquals (ref "s3expressAvailabilityZoneDelim") "--")
            (rules
             (conditions
              (booleanEquals (ref "UseFIPS") #t)
              (endpoint
               "https://{Bucket}.s3express-fips-{s3expressAvailabilityZoneId}.{Region}.{partitionResult#dnsSuffix}"))
             (conditions
              (endpoint
               "https://{Bucket}.s3express-{s3expressAvailabilityZoneId}.{Region}.{partitionResult#dnsSuffix}"))))
           (conditions
            (assign "s3expressAvailabilityZoneId" (substring (ref "Bucket") 6 20 #t))
            (assign "s3expressAvailabilityZoneDelim" (substring (ref "Bucket") 20 22 #t))
            (stringEquals (ref "s3expressAvailabilityZoneDelim") "--")
            (rules
             (conditions
              (booleanEquals (ref "UseFIPS") #t)
              (endpoint
               "https://{Bucket}.s3express-fips-{s3expressAvailabilityZoneId}.{Region}.{partitionResult#dnsSuffix}"))
             (conditions
              (endpoint
               "https://{Bucket}.s3express-{s3expressAvailabilityZoneId}.{Region}.{partitionResult#dnsSuffix}"))))
           (conditions
            (assign "s3expressAvailabilityZoneId" (substring (ref "Bucket") 6 26 #t))
            (assign "s3expressAvailabilityZoneDelim" (substring (ref "Bucket") 26 28 #t))
            (stringEquals (ref "s3expressAvailabilityZoneDelim") "--")
            (rules
             (conditions
              (booleanEquals (ref "UseFIPS") #t)
              (endpoint
               "https://{Bucket}.s3express-fips-{s3expressAvailabilityZoneId}.{Region}.{partitionResult#dnsSuffix}"))
             (conditions
              (endpoint
               "https://{Bucket}.s3express-{s3expressAvailabilityZoneId}.{Region}.{partitionResult#dnsSuffix}"))))
           (conditions (error "Unrecognized S3Express bucket name format."))))))
       (conditions (error "S3Express bucket name is not a valid virtual hostable name."))))
     (conditions
      (isSet (ref "Bucket"))
      (assign "accessPointSuffix" (substring (ref "Bucket") 0 7 #t))
      (stringEquals (ref "accessPointSuffix") "--xa-s3")
      (rules
       (conditions (booleanEquals (ref "UseDualStack") #t)
                   (error "S3Express does not support Dual-stack."))
       (conditions (booleanEquals (ref "Accelerate") #t)
                   (error "S3Express does not support S3 Accelerate."))
       (conditions
        (isSet (ref "Endpoint"))
        (assign "url" (parseURL (ref "Endpoint")))
        (rules
         (conditions
          (isSet (ref "DisableS3ExpressSessionAuth"))
          (booleanEquals (ref "DisableS3ExpressSessionAuth") #t)
          (rules
           (conditions
            (booleanEquals (getAttr (ref "url") "isIp") #t)
            (rules (conditions
                    (assign "uri_encoded_bucket" (uriEncode (ref "Bucket")))
                    (rules (conditions
                            (endpoint
                             "{url#scheme}://{url#authority}/{uri_encoded_bucket}{url#path}"))))))
           (conditions
            (aws.isVirtualHostableS3Bucket (ref "Bucket") #f)
            (rules (conditions (endpoint "{url#scheme}://{Bucket}.{url#authority}{url#path}"))))
           (conditions (error "S3Express bucket name is not a valid virtual hostable name."))))
         (conditions
          (booleanEquals (getAttr (ref "url") "isIp") #t)
          (rules (conditions
                  (assign "uri_encoded_bucket" (uriEncode (ref "Bucket")))
                  (rules (conditions
                          (endpoint
                           "{url#scheme}://{url#authority}/{uri_encoded_bucket}{url#path}"))))))
         (conditions
          (aws.isVirtualHostableS3Bucket (ref "Bucket") #f)
          (rules (conditions (endpoint "{url#scheme}://{Bucket}.{url#authority}{url#path}"))))
         (conditions (error "S3Express bucket name is not a valid virtual hostable name."))))
       (conditions
        (aws.isVirtualHostableS3Bucket (ref "Bucket") #f)
        (rules
         (conditions
          (assign "partitionResult" (aws.partition (ref "Region")))
          (rules
           (conditions
            (isSet (ref "DisableS3ExpressSessionAuth"))
            (booleanEquals (ref "DisableS3ExpressSessionAuth") #t)
            (rules
             (conditions
              (assign "s3expressAvailabilityZoneId" (substring (ref "Bucket") 7 15 #t))
              (assign "s3expressAvailabilityZoneDelim" (substring (ref "Bucket") 15 17 #t))
              (stringEquals (ref "s3expressAvailabilityZoneDelim") "--")
              (rules
               (conditions
                (booleanEquals (ref "UseFIPS") #t)
                (endpoint
                 "https://{Bucket}.s3express-fips-{s3expressAvailabilityZoneId}.{Region}.{partitionResult#dnsSuffix}"))
               (conditions
                (endpoint
                 "https://{Bucket}.s3express-{s3expressAvailabilityZoneId}.{Region}.{partitionResult#dnsSuffix}"))))
             (conditions
              (assign "s3expressAvailabilityZoneId" (substring (ref "Bucket") 7 16 #t))
              (assign "s3expressAvailabilityZoneDelim" (substring (ref "Bucket") 16 18 #t))
              (stringEquals (ref "s3expressAvailabilityZoneDelim") "--")
              (rules
               (conditions
                (booleanEquals (ref "UseFIPS") #t)
                (endpoint
                 "https://{Bucket}.s3express-fips-{s3expressAvailabilityZoneId}.{Region}.{partitionResult#dnsSuffix}"))
               (conditions
                (endpoint
                 "https://{Bucket}.s3express-{s3expressAvailabilityZoneId}.{Region}.{partitionResult#dnsSuffix}"))))
             (conditions
              (assign "s3expressAvailabilityZoneId" (substring (ref "Bucket") 7 20 #t))
              (assign "s3expressAvailabilityZoneDelim" (substring (ref "Bucket") 20 22 #t))
              (stringEquals (ref "s3expressAvailabilityZoneDelim") "--")
              (rules
               (conditions
                (booleanEquals (ref "UseFIPS") #t)
                (endpoint
                 "https://{Bucket}.s3express-fips-{s3expressAvailabilityZoneId}.{Region}.{partitionResult#dnsSuffix}"))
               (conditions
                (endpoint
                 "https://{Bucket}.s3express-{s3expressAvailabilityZoneId}.{Region}.{partitionResult#dnsSuffix}"))))
             (conditions
              (assign "s3expressAvailabilityZoneId" (substring (ref "Bucket") 7 21 #t))
              (assign "s3expressAvailabilityZoneDelim" (substring (ref "Bucket") 21 23 #t))
              (stringEquals (ref "s3expressAvailabilityZoneDelim") "--")
              (rules
               (conditions
                (booleanEquals (ref "UseFIPS") #t)
                (endpoint
                 "https://{Bucket}.s3express-fips-{s3expressAvailabilityZoneId}.{Region}.{partitionResult#dnsSuffix}"))
               (conditions
                (endpoint
                 "https://{Bucket}.s3express-{s3expressAvailabilityZoneId}.{Region}.{partitionResult#dnsSuffix}"))))
             (conditions
              (assign "s3expressAvailabilityZoneId" (substring (ref "Bucket") 7 27 #t))
              (assign "s3expressAvailabilityZoneDelim" (substring (ref "Bucket") 27 29 #t))
              (stringEquals (ref "s3expressAvailabilityZoneDelim") "--")
              (rules
               (conditions
                (booleanEquals (ref "UseFIPS") #t)
                (endpoint
                 "https://{Bucket}.s3express-fips-{s3expressAvailabilityZoneId}.{Region}.{partitionResult#dnsSuffix}"))
               (conditions
                (endpoint
                 "https://{Bucket}.s3express-{s3expressAvailabilityZoneId}.{Region}.{partitionResult#dnsSuffix}"))))
             (conditions (error "Unrecognized S3Express bucket name format."))))
           (conditions
            (assign "s3expressAvailabilityZoneId" (substring (ref "Bucket") 7 15 #t))
            (assign "s3expressAvailabilityZoneDelim" (substring (ref "Bucket") 15 17 #t))
            (stringEquals (ref "s3expressAvailabilityZoneDelim") "--")
            (rules
             (conditions
              (booleanEquals (ref "UseFIPS") #t)
              (endpoint
               "https://{Bucket}.s3express-fips-{s3expressAvailabilityZoneId}.{Region}.{partitionResult#dnsSuffix}"))
             (conditions
              (endpoint
               "https://{Bucket}.s3express-{s3expressAvailabilityZoneId}.{Region}.{partitionResult#dnsSuffix}"))))
           (conditions
            (assign "s3expressAvailabilityZoneId" (substring (ref "Bucket") 7 16 #t))
            (assign "s3expressAvailabilityZoneDelim" (substring (ref "Bucket") 16 18 #t))
            (stringEquals (ref "s3expressAvailabilityZoneDelim") "--")
            (rules
             (conditions
              (booleanEquals (ref "UseFIPS") #t)
              (endpoint
               "https://{Bucket}.s3express-fips-{s3expressAvailabilityZoneId}.{Region}.{partitionResult#dnsSuffix}"))
             (conditions
              (endpoint
               "https://{Bucket}.s3express-{s3expressAvailabilityZoneId}.{Region}.{partitionResult#dnsSuffix}"))))
           (conditions
            (assign "s3expressAvailabilityZoneId" (substring (ref "Bucket") 7 20 #t))
            (assign "s3expressAvailabilityZoneDelim" (substring (ref "Bucket") 20 22 #t))
            (stringEquals (ref "s3expressAvailabilityZoneDelim") "--")
            (rules
             (conditions
              (booleanEquals (ref "UseFIPS") #t)
              (endpoint
               "https://{Bucket}.s3express-fips-{s3expressAvailabilityZoneId}.{Region}.{partitionResult#dnsSuffix}"))
             (conditions
              (endpoint
               "https://{Bucket}.s3express-{s3expressAvailabilityZoneId}.{Region}.{partitionResult#dnsSuffix}"))))
           (conditions
            (assign "s3expressAvailabilityZoneId" (substring (ref "Bucket") 7 21 #t))
            (assign "s3expressAvailabilityZoneDelim" (substring (ref "Bucket") 21 23 #t))
            (stringEquals (ref "s3expressAvailabilityZoneDelim") "--")
            (rules
             (conditions
              (booleanEquals (ref "UseFIPS") #t)
              (endpoint
               "https://{Bucket}.s3express-fips-{s3expressAvailabilityZoneId}.{Region}.{partitionResult#dnsSuffix}"))
             (conditions
              (endpoint
               "https://{Bucket}.s3express-{s3expressAvailabilityZoneId}.{Region}.{partitionResult#dnsSuffix}"))))
           (conditions
            (assign "s3expressAvailabilityZoneId" (substring (ref "Bucket") 7 27 #t))
            (assign "s3expressAvailabilityZoneDelim" (substring (ref "Bucket") 27 29 #t))
            (stringEquals (ref "s3expressAvailabilityZoneDelim") "--")
            (rules
             (conditions
              (booleanEquals (ref "UseFIPS") #t)
              (endpoint
               "https://{Bucket}.s3express-fips-{s3expressAvailabilityZoneId}.{Region}.{partitionResult#dnsSuffix}"))
             (conditions
              (endpoint
               "https://{Bucket}.s3express-{s3expressAvailabilityZoneId}.{Region}.{partitionResult#dnsSuffix}"))))
           (conditions (error "Unrecognized S3Express bucket name format."))))))
       (conditions (error "S3Express bucket name is not a valid virtual hostable name."))))
     (conditions
      (not (isSet (ref "Bucket")))
      (isSet (ref "UseS3ExpressControlEndpoint"))
      (booleanEquals (ref "UseS3ExpressControlEndpoint") #t)
      (rules
       (conditions
        (assign "partitionResult" (aws.partition (ref "Region")))
        (rules
         (conditions (isSet (ref "Endpoint"))
                     (assign "url" (parseURL (ref "Endpoint")))
                     (endpoint "{url#scheme}://{url#authority}{url#path}"))
         (conditions (booleanEquals (ref "UseFIPS") #t)
                     (endpoint "https://s3express-control-fips.{Region}.{partitionResult#dnsSuffix}"))
         (conditions (endpoint "https://s3express-control.{Region}.{partitionResult#dnsSuffix}"))))))
     (conditions
      (isSet (ref "Bucket"))
      (assign "hardwareType" (substring (ref "Bucket") 49 50 #t))
      (assign "regionPrefix" (substring (ref "Bucket") 8 12 #t))
      (assign "bucketAliasSuffix" (substring (ref "Bucket") 0 7 #t))
      (assign "outpostId" (substring (ref "Bucket") 32 49 #t))
      (assign "regionPartition" (aws.partition (ref "Region")))
      (stringEquals (ref "bucketAliasSuffix") "--op-s3")
      (rules
       (conditions
        (isValidHostLabel (ref "outpostId") #f)
        (rules
         (conditions
          (stringEquals (ref "hardwareType") "e")
          (rules (conditions
                  (stringEquals (ref "regionPrefix") "beta")
                  (rules (conditions
                          (not (isSet (ref "Endpoint")))
                          (error "Expected a endpoint to be specified but no endpoint was found"))
                         (conditions (isSet (ref "Endpoint"))
                                     (assign "url" (parseURL (ref "Endpoint")))
                                     (endpoint "https://{Bucket}.ec2.{url#authority}"))))
                 (conditions
                  (endpoint
                   "https://{Bucket}.ec2.s3-outposts.{Region}.{regionPartition#dnsSuffix}"))))
         (conditions
          (stringEquals (ref "hardwareType") "o")
          (rules
           (conditions
            (stringEquals (ref "regionPrefix") "beta")
            (rules
             (conditions (not (isSet (ref "Endpoint")))
                         (error "Expected a endpoint to be specified but no endpoint was found"))
             (conditions (isSet (ref "Endpoint"))
                         (assign "url" (parseURL (ref "Endpoint")))
                         (endpoint "https://{Bucket}.op-{outpostId}.{url#authority}"))))
           (conditions
            (endpoint
             "https://{Bucket}.op-{outpostId}.s3-outposts.{Region}.{regionPartition#dnsSuffix}"))))
         (conditions
          (error
           "Unrecognized hardware type: \"Expected hardware type o or e but got {hardwareType}\""))))
       (conditions (error "Invalid ARN: The outpost Id must only contain a-z, A-Z, 0-9 and `-`."))))
     (conditions
      (isSet (ref "Bucket"))
      (rules
       (conditions (isSet (ref "Endpoint"))
                   (not (isSet (parseURL (ref "Endpoint"))))
                   (error "Custom endpoint `{Endpoint}` was not a valid URI"))
       (conditions
        (booleanEquals (ref "ForcePathStyle") #f)
        (aws.isVirtualHostableS3Bucket (ref "Bucket") #f)
        (rules
         (conditions
          (assign "partitionResult" (aws.partition (ref "Region")))
          (rules
           (conditions
            (isValidHostLabel (ref "Region") #f)
            (rules
             (conditions (booleanEquals (ref "Accelerate") #t)
                         (stringEquals (getAttr (ref "partitionResult") "name") "aws-cn")
                         (error "S3 Accelerate cannot be used in this region"))
             (conditions
              (booleanEquals (ref "UseDualStack") #t)
              (booleanEquals (ref "UseFIPS") #t)
              (booleanEquals (ref "Accelerate") #f)
              (not (isSet (ref "Endpoint")))
              (stringEquals (ref "Region") "aws-global")
              (endpoint "https://{Bucket}.s3-fips.dualstack.us-east-1.{partitionResult#dnsSuffix}"))
             (conditions
              (booleanEquals (ref "UseDualStack") #t)
              (booleanEquals (ref "UseFIPS") #t)
              (booleanEquals (ref "Accelerate") #f)
              (not (isSet (ref "Endpoint")))
              (not (stringEquals (ref "Region") "aws-global"))
              (booleanEquals (ref "UseGlobalEndpoint") #t)
              (rules (conditions
                      (endpoint
                       "https://{Bucket}.s3-fips.dualstack.{Region}.{partitionResult#dnsSuffix}"))))
             (conditions
              (booleanEquals (ref "UseDualStack") #t)
              (booleanEquals (ref "UseFIPS") #t)
              (booleanEquals (ref "Accelerate") #f)
              (not (isSet (ref "Endpoint")))
              (not (stringEquals (ref "Region") "aws-global"))
              (booleanEquals (ref "UseGlobalEndpoint") #f)
              (endpoint "https://{Bucket}.s3-fips.dualstack.{Region}.{partitionResult#dnsSuffix}"))
             (conditions (booleanEquals (ref "UseDualStack") #f)
                         (booleanEquals (ref "UseFIPS") #t)
                         (booleanEquals (ref "Accelerate") #f)
                         (not (isSet (ref "Endpoint")))
                         (stringEquals (ref "Region") "aws-global")
                         (endpoint "https://{Bucket}.s3-fips.us-east-1.{partitionResult#dnsSuffix}"))
             (conditions
              (booleanEquals (ref "UseDualStack") #f)
              (booleanEquals (ref "UseFIPS") #t)
              (booleanEquals (ref "Accelerate") #f)
              (not (isSet (ref "Endpoint")))
              (not (stringEquals (ref "Region") "aws-global"))
              (booleanEquals (ref "UseGlobalEndpoint") #t)
              (rules (conditions
                      (endpoint "https://{Bucket}.s3-fips.{Region}.{partitionResult#dnsSuffix}"))))
             (conditions (booleanEquals (ref "UseDualStack") #f)
                         (booleanEquals (ref "UseFIPS") #t)
                         (booleanEquals (ref "Accelerate") #f)
                         (not (isSet (ref "Endpoint")))
                         (not (stringEquals (ref "Region") "aws-global"))
                         (booleanEquals (ref "UseGlobalEndpoint") #f)
                         (endpoint "https://{Bucket}.s3-fips.{Region}.{partitionResult#dnsSuffix}"))
             (conditions
              (booleanEquals (ref "UseDualStack") #t)
              (booleanEquals (ref "UseFIPS") #f)
              (booleanEquals (ref "Accelerate") #t)
              (not (isSet (ref "Endpoint")))
              (stringEquals (ref "Region") "aws-global")
              (endpoint
               "https://{Bucket}.s3-accelerate.dualstack.us-east-1.{partitionResult#dnsSuffix}"))
             (conditions
              (booleanEquals (ref "UseDualStack") #t)
              (booleanEquals (ref "UseFIPS") #f)
              (booleanEquals (ref "Accelerate") #t)
              (not (isSet (ref "Endpoint")))
              (not (stringEquals (ref "Region") "aws-global"))
              (booleanEquals (ref "UseGlobalEndpoint") #t)
              (rules (conditions
                      (endpoint
                       "https://{Bucket}.s3-accelerate.dualstack.{partitionResult#dnsSuffix}"))))
             (conditions
              (booleanEquals (ref "UseDualStack") #t)
              (booleanEquals (ref "UseFIPS") #f)
              (booleanEquals (ref "Accelerate") #t)
              (not (isSet (ref "Endpoint")))
              (not (stringEquals (ref "Region") "aws-global"))
              (booleanEquals (ref "UseGlobalEndpoint") #f)
              (endpoint "https://{Bucket}.s3-accelerate.dualstack.{partitionResult#dnsSuffix}"))
             (conditions
              (booleanEquals (ref "UseDualStack") #t)
              (booleanEquals (ref "UseFIPS") #f)
              (booleanEquals (ref "Accelerate") #f)
              (not (isSet (ref "Endpoint")))
              (stringEquals (ref "Region") "aws-global")
              (endpoint "https://{Bucket}.s3.dualstack.us-east-1.{partitionResult#dnsSuffix}"))
             (conditions
              (booleanEquals (ref "UseDualStack") #t)
              (booleanEquals (ref "UseFIPS") #f)
              (booleanEquals (ref "Accelerate") #f)
              (not (isSet (ref "Endpoint")))
              (not (stringEquals (ref "Region") "aws-global"))
              (booleanEquals (ref "UseGlobalEndpoint") #t)
              (rules (conditions
                      (endpoint
                       "https://{Bucket}.s3.dualstack.{Region}.{partitionResult#dnsSuffix}"))))
             (conditions
              (booleanEquals (ref "UseDualStack") #t)
              (booleanEquals (ref "UseFIPS") #f)
              (booleanEquals (ref "Accelerate") #f)
              (not (isSet (ref "Endpoint")))
              (not (stringEquals (ref "Region") "aws-global"))
              (booleanEquals (ref "UseGlobalEndpoint") #f)
              (endpoint "https://{Bucket}.s3.dualstack.{Region}.{partitionResult#dnsSuffix}"))
             (conditions (booleanEquals (ref "UseDualStack") #f)
                         (booleanEquals (ref "UseFIPS") #f)
                         (booleanEquals (ref "Accelerate") #f)
                         (isSet (ref "Endpoint"))
                         (assign "url" (parseURL (ref "Endpoint")))
                         (booleanEquals (getAttr (ref "url") "isIp") #t)
                         (stringEquals (ref "Region") "aws-global")
                         (endpoint "{url#scheme}://{url#authority}{url#normalizedPath}{Bucket}"))
             (conditions (booleanEquals (ref "UseDualStack") #f)
                         (booleanEquals (ref "UseFIPS") #f)
                         (booleanEquals (ref "Accelerate") #f)
                         (isSet (ref "Endpoint"))
                         (assign "url" (parseURL (ref "Endpoint")))
                         (booleanEquals (getAttr (ref "url") "isIp") #f)
                         (stringEquals (ref "Region") "aws-global")
                         (endpoint "{url#scheme}://{Bucket}.{url#authority}{url#path}"))
             (conditions
              (booleanEquals (ref "UseDualStack") #f)
              (booleanEquals (ref "UseFIPS") #f)
              (booleanEquals (ref "Accelerate") #f)
              (isSet (ref "Endpoint"))
              (assign "url" (parseURL (ref "Endpoint")))
              (booleanEquals (getAttr (ref "url") "isIp") #t)
              (not (stringEquals (ref "Region") "aws-global"))
              (booleanEquals (ref "UseGlobalEndpoint") #t)
              (rules
               (conditions (stringEquals (ref "Region") "us-east-1")
                           (endpoint "{url#scheme}://{url#authority}{url#normalizedPath}{Bucket}"))
               (conditions (endpoint "{url#scheme}://{url#authority}{url#normalizedPath}{Bucket}"))))
             (conditions
              (booleanEquals (ref "UseDualStack") #f)
              (booleanEquals (ref "UseFIPS") #f)
              (booleanEquals (ref "Accelerate") #f)
              (isSet (ref "Endpoint"))
              (assign "url" (parseURL (ref "Endpoint")))
              (booleanEquals (getAttr (ref "url") "isIp") #f)
              (not (stringEquals (ref "Region") "aws-global"))
              (booleanEquals (ref "UseGlobalEndpoint") #t)
              (rules (conditions (stringEquals (ref "Region") "us-east-1")
                                 (endpoint "{url#scheme}://{Bucket}.{url#authority}{url#path}"))
                     (conditions (endpoint "{url#scheme}://{Bucket}.{url#authority}{url#path}"))))
             (conditions (booleanEquals (ref "UseDualStack") #f)
                         (booleanEquals (ref "UseFIPS") #f)
                         (booleanEquals (ref "Accelerate") #f)
                         (isSet (ref "Endpoint"))
                         (assign "url" (parseURL (ref "Endpoint")))
                         (booleanEquals (getAttr (ref "url") "isIp") #t)
                         (not (stringEquals (ref "Region") "aws-global"))
                         (booleanEquals (ref "UseGlobalEndpoint") #f)
                         (endpoint "{url#scheme}://{url#authority}{url#normalizedPath}{Bucket}"))
             (conditions (booleanEquals (ref "UseDualStack") #f)
                         (booleanEquals (ref "UseFIPS") #f)
                         (booleanEquals (ref "Accelerate") #f)
                         (isSet (ref "Endpoint"))
                         (assign "url" (parseURL (ref "Endpoint")))
                         (booleanEquals (getAttr (ref "url") "isIp") #f)
                         (not (stringEquals (ref "Region") "aws-global"))
                         (booleanEquals (ref "UseGlobalEndpoint") #f)
                         (endpoint "{url#scheme}://{Bucket}.{url#authority}{url#path}"))
             (conditions (booleanEquals (ref "UseDualStack") #f)
                         (booleanEquals (ref "UseFIPS") #f)
                         (booleanEquals (ref "Accelerate") #t)
                         (not (isSet (ref "Endpoint")))
                         (stringEquals (ref "Region") "aws-global")
                         (endpoint "https://{Bucket}.s3-accelerate.{partitionResult#dnsSuffix}"))
             (conditions
              (booleanEquals (ref "UseDualStack") #f)
              (booleanEquals (ref "UseFIPS") #f)
              (booleanEquals (ref "Accelerate") #t)
              (not (isSet (ref "Endpoint")))
              (not (stringEquals (ref "Region") "aws-global"))
              (booleanEquals (ref "UseGlobalEndpoint") #t)
              (rules
               (conditions (stringEquals (ref "Region") "us-east-1")
                           (endpoint "https://{Bucket}.s3-accelerate.{partitionResult#dnsSuffix}"))
               (conditions (endpoint "https://{Bucket}.s3-accelerate.{partitionResult#dnsSuffix}"))))
             (conditions (booleanEquals (ref "UseDualStack") #f)
                         (booleanEquals (ref "UseFIPS") #f)
                         (booleanEquals (ref "Accelerate") #t)
                         (not (isSet (ref "Endpoint")))
                         (not (stringEquals (ref "Region") "aws-global"))
                         (booleanEquals (ref "UseGlobalEndpoint") #f)
                         (endpoint "https://{Bucket}.s3-accelerate.{partitionResult#dnsSuffix}"))
             (conditions (booleanEquals (ref "UseDualStack") #f)
                         (booleanEquals (ref "UseFIPS") #f)
                         (booleanEquals (ref "Accelerate") #f)
                         (not (isSet (ref "Endpoint")))
                         (stringEquals (ref "Region") "aws-global")
                         (endpoint "https://{Bucket}.s3.{partitionResult#dnsSuffix}"))
             (conditions
              (booleanEquals (ref "UseDualStack") #f)
              (booleanEquals (ref "UseFIPS") #f)
              (booleanEquals (ref "Accelerate") #f)
              (not (isSet (ref "Endpoint")))
              (not (stringEquals (ref "Region") "aws-global"))
              (booleanEquals (ref "UseGlobalEndpoint") #t)
              (rules (conditions (stringEquals (ref "Region") "us-east-1")
                                 (endpoint "https://{Bucket}.s3.{partitionResult#dnsSuffix}"))
                     (conditions
                      (endpoint "https://{Bucket}.s3.{Region}.{partitionResult#dnsSuffix}"))))
             (conditions (booleanEquals (ref "UseDualStack") #f)
                         (booleanEquals (ref "UseFIPS") #f)
                         (booleanEquals (ref "Accelerate") #f)
                         (not (isSet (ref "Endpoint")))
                         (not (stringEquals (ref "Region") "aws-global"))
                         (booleanEquals (ref "UseGlobalEndpoint") #f)
                         (endpoint "https://{Bucket}.s3.{Region}.{partitionResult#dnsSuffix}"))))
           (conditions (error "Invalid region: region was not a valid DNS name."))))))
       (conditions
        (isSet (ref "Endpoint"))
        (assign "url" (parseURL (ref "Endpoint")))
        (stringEquals (getAttr (ref "url") "scheme") "http")
        (aws.isVirtualHostableS3Bucket (ref "Bucket") #t)
        (booleanEquals (ref "ForcePathStyle") #f)
        (booleanEquals (ref "UseFIPS") #f)
        (booleanEquals (ref "UseDualStack") #f)
        (booleanEquals (ref "Accelerate") #f)
        (rules
         (conditions
          (assign "partitionResult" (aws.partition (ref "Region")))
          (rules (conditions
                  (isValidHostLabel (ref "Region") #f)
                  (rules (conditions (endpoint "{url#scheme}://{Bucket}.{url#authority}{url#path}"))))
                 (conditions (error "Invalid region: region was not a valid DNS name."))))))
       (conditions
        (booleanEquals (ref "ForcePathStyle") #f)
        (assign "bucketArn" (aws.parseArn (ref "Bucket")))
        (rules
         (conditions
          (assign "arnType" (getAttr (ref "bucketArn") "resourceId[0]"))
          (stringEquals (ref "arnType") "")
          ;; (not (stringEquals (ref "arnType") ""))
          (rules
           (conditions
            (stringEquals (getAttr (ref "bucketArn") "service") "s3-object-lambda")
            (rules
             (conditions
              (stringEquals (ref "arnType") "accesspoint")
              (rules
               (conditions
                (assign "accessPointName" (getAttr (ref "bucketArn") "resourceId[1]"))
                (not (stringEquals (ref "accessPointName") ""))
                (rules
                 (conditions (booleanEquals (ref "UseDualStack") #t)
                             (error "S3 Object Lambda does not support Dual-stack"))
                 (conditions (booleanEquals (ref "Accelerate") #t)
                             (error "S3 Object Lambda does not support S3 Accelerate"))
                 (conditions
                  (not (stringEquals (getAttr (ref "bucketArn") "region") ""))
                  (rules
                   (conditions (isSet (ref "DisableAccessPoints"))
                               (booleanEquals (ref "DisableAccessPoints") #t)
                               (error "Access points are not supported for this operation"))
                   (conditions
                    (not (isSet (getAttr (ref "bucketArn") "resourceId[2]")))
                    (rules
                     (conditions
                      (isSet (ref "UseArnRegion"))
                      (booleanEquals (ref "UseArnRegion") #f)
                      (not (stringEquals (getAttr (ref "bucketArn") "region") "{Region}"))
                      (error
                       "Invalid configuration: region from ARN `{bucketArn#region}` does not match client region `{Region}` and UseArnRegion is `false`"))
                     (conditions
                      (assign "bucketPartition" (aws.partition (getAttr (ref "bucketArn") "region")))
                      (rules
                       (conditions
                        (assign "partitionResult" (aws.partition (ref "Region")))
                        (rules
                         (conditions
                          (stringEquals (getAttr (ref "bucketPartition") "name")
                                        (getAttr (ref "partitionResult") "name"))
                          (rules
                           (conditions
                            (isValidHostLabel (getAttr (ref "bucketArn") "region") #t)
                            (rules
                             (conditions (stringEquals (getAttr (ref "bucketArn") "accountId") "")
                                         (error "Invalid ARN: Missing account id"))
                             (conditions
                              (isValidHostLabel (getAttr (ref "bucketArn") "accountId") #f)
                              (rules
                               (conditions
                                (isValidHostLabel (ref "accessPointName") #f)
                                (rules
                                 (conditions
                                  (isSet (ref "Endpoint"))
                                  (assign "url" (parseURL (ref "Endpoint")))
                                  (endpoint
                                   "{url#scheme}://{accessPointName}-{bucketArn#accountId}.{url#authority}{url#path}"))
                                 (conditions
                                  (booleanEquals (ref "UseFIPS") #t)
                                  (endpoint
                                   "https://{accessPointName}-{bucketArn#accountId}.s3-object-lambda-fips.{bucketArn#region}.{bucketPartition#dnsSuffix}"))
                                 (conditions
                                  (endpoint
                                   "https://{accessPointName}-{bucketArn#accountId}.s3-object-lambda.{bucketArn#region}.{bucketPartition#dnsSuffix}"))))
                               (conditions
                                (error
                                 "Invalid ARN: The access point name may only contain a-z, A-Z, 0-9 and `-`. Found: `{accessPointName}`"))))
                             (conditions
                              (error
                               "Invalid ARN: The account id may only contain a-z, A-Z, 0-9 and `-`. Found: `{bucketArn#accountId}`"))))
                           (conditions
                            (error "Invalid region in ARN: `{bucketArn#region}` (invalid DNS name)"))))
                         (conditions
                          (error
                           "Client was configured for partition `{partitionResult#name}` but ARN (`{Bucket}`) has `{bucketPartition#name}`"))))))))
                   (conditions
                    (error
                     "Invalid ARN: The ARN may only contain a single resource component after `accesspoint`."))))
                 (conditions (error "Invalid ARN: bucket ARN is missing a region"))))
               (conditions
                (error
                 "Invalid ARN: Expected a resource of the format `accesspoint:<accesspoint name>` but no name was provided"))))
             (conditions
              (error
               "Invalid ARN: Object Lambda ARNs only support `accesspoint` arn types, but found: `{arnType}`"))))
           (conditions
            (stringEquals (ref "arnType") "accesspoint")
            (rules
             (conditions
              (assign "accessPointName" (getAttr (ref "bucketArn") "resourceId[1]"))
              (not (stringEquals (ref "accessPointName") ""))
              (rules
               (conditions
                (not (stringEquals (getAttr (ref "bucketArn") "region") ""))
                (rules
                 (conditions
                  (stringEquals (ref "arnType") "accesspoint")
                  (rules
                   (conditions
                    (not (stringEquals (getAttr (ref "bucketArn") "region") ""))
                    (rules
                     (conditions (isSet (ref "DisableAccessPoints"))
                                 (booleanEquals (ref "DisableAccessPoints") #t)
                                 (error "Access points are not supported for this operation"))
                     (conditions
                      (not (isSet (getAttr (ref "bucketArn") "resourceId[2]")))
                      (rules
                       (conditions
                        (isSet (ref "UseArnRegion"))
                        (booleanEquals (ref "UseArnRegion") #f)
                        (not (stringEquals (getAttr (ref "bucketArn") "region") "{Region}"))
                        (error
                         "Invalid configuration: region from ARN `{bucketArn#region}` does not match client region `{Region}` and UseArnRegion is `false`"))
                       (conditions
                        (assign "bucketPartition"
                                (aws.partition (getAttr (ref "bucketArn") "region")))
                        (rules
                         (conditions
                          (assign "partitionResult" (aws.partition (ref "Region")))
                          (rules
                           (conditions
                            (stringEquals (getAttr (ref "bucketPartition") "name")
                                          "{partitionResult#name}")
                            (rules
                             (conditions
                              (isValidHostLabel (getAttr (ref "bucketArn") "region") #t)
                              (rules
                               (conditions
                                (stringEquals (getAttr (ref "bucketArn") "service") "s3")
                                (rules
                                 (conditions
                                  (isValidHostLabel (getAttr (ref "bucketArn") "accountId") #f)
                                  (rules
                                   (conditions
                                    (isValidHostLabel (ref "accessPointName") #f)
                                    (rules
                                     (conditions (booleanEquals (ref "Accelerate") #t)
                                                 (error "Access Points do not support S3 Accelerate"))
                                     (conditions
                                      (booleanEquals (ref "UseFIPS") #t)
                                      (booleanEquals (ref "UseDualStack") #t)
                                      (endpoint
                                       "https://{accessPointName}-{bucketArn#accountId}.s3-accesspoint-fips.dualstack.{bucketArn#region}.{bucketPartition#dnsSuffix}"))
                                     (conditions
                                      (booleanEquals (ref "UseFIPS") #t)
                                      (booleanEquals (ref "UseDualStack") #f)
                                      (endpoint
                                       "https://{accessPointName}-{bucketArn#accountId}.s3-accesspoint-fips.{bucketArn#region}.{bucketPartition#dnsSuffix}"))
                                     (conditions
                                      (booleanEquals (ref "UseFIPS") #f)
                                      (booleanEquals (ref "UseDualStack") #t)
                                      (endpoint
                                       "https://{accessPointName}-{bucketArn#accountId}.s3-accesspoint.dualstack.{bucketArn#region}.{bucketPartition#dnsSuffix}"))
                                     (conditions
                                      (booleanEquals (ref "UseFIPS") #f)
                                      (booleanEquals (ref "UseDualStack") #f)
                                      (isSet (ref "Endpoint"))
                                      (assign "url" (parseURL (ref "Endpoint")))
                                      (endpoint
                                       "{url#scheme}://{accessPointName}-{bucketArn#accountId}.{url#authority}{url#path}"))
                                     (conditions
                                      (booleanEquals (ref "UseFIPS") #f)
                                      (booleanEquals (ref "UseDualStack") #f)
                                      (endpoint
                                       "https://{accessPointName}-{bucketArn#accountId}.s3-accesspoint.{bucketArn#region}.{bucketPartition#dnsSuffix}"))))
                                   (conditions
                                    (error
                                     "Invalid ARN: The access point name may only contain a-z, A-Z, 0-9 and `-`. Found: `{accessPointName}`"))))
                                 (conditions
                                  (error
                                   "Invalid ARN: The account id may only contain a-z, A-Z, 0-9 and `-`. Found: `{bucketArn#accountId}`"))))
                               (conditions
                                (error
                                 "Invalid ARN: The ARN was not for the S3 service, found: {bucketArn#service}"))))
                             (conditions
                              (error
                               "Invalid region in ARN: `{bucketArn#region}` (invalid DNS name)"))))
                           (conditions
                            (error
                             "Client was configured for partition `{partitionResult#name}` but ARN (`{Bucket}`) has `{bucketPartition#name}`"))))))))
                     (conditions
                      (error
                       "Invalid ARN: The ARN may only contain a single resource component after `accesspoint`."))))))))
               (conditions
                (isValidHostLabel (ref "accessPointName") #t)
                (rules
                 (conditions (booleanEquals (ref "UseDualStack") #t)
                             (error "S3 MRAP does not support dual-stack"))
                 (conditions (booleanEquals (ref "UseFIPS") #t)
                             (error "S3 MRAP does not support FIPS"))
                 (conditions (booleanEquals (ref "Accelerate") #t)
                             (error "S3 MRAP does not support S3 Accelerate"))
                 (conditions
                  (booleanEquals (ref "DisableMultiRegionAccessPoints") #t)
                  (error "Invalid configuration: Multi-Region Access Point ARNs are disabled."))
                 (conditions
                  (assign "mrapPartition" (aws.partition (ref "Region")))
                  (rules
                   (conditions
                    (stringEquals (getAttr (ref "mrapPartition") "name")
                                  (getAttr (ref "bucketArn") "partition"))
                    (rules
                     (conditions
                      (endpoint
                       "https://{accessPointName}.accesspoint.s3-global.{mrapPartition#dnsSuffix}"))))
                   (conditions
                    (error
                     "Client was configured for partition `{mrapPartition#name}` but bucket referred to partition `{bucketArn#partition}`"))))))
               (conditions (error "Invalid Access Point Name"))))
             (conditions
              (error
               "Invalid ARN: Expected a resource of the format `accesspoint:<accesspoint name>` but no name was provided"))))
           (conditions
            (stringEquals (getAttr (ref "bucketArn") "service") "s3-outposts")
            (rules
             (conditions (booleanEquals (ref "UseDualStack") #t)
                         (error "S3 Outposts does not support Dual-stack"))
             (conditions (booleanEquals (ref "UseFIPS") #t)
                         (error "S3 Outposts does not support FIPS"))
             (conditions (booleanEquals (ref "Accelerate") #t)
                         (error "S3 Outposts does not support S3 Accelerate"))
             (conditions (isSet (getAttr (ref "bucketArn") "resourceId[4]"))
                         (error "Invalid Arn: Outpost Access Point ARN contains sub resources"))
             (conditions
              (assign "outpostId" (getAttr (ref "bucketArn") "resourceId[1]"))
              (rules
               (conditions
                (isValidHostLabel (ref "outpostId") #f)
                (rules
                 (conditions
                  (isSet (ref "UseArnRegion"))
                  (booleanEquals (ref "UseArnRegion") #f)
                  (not (stringEquals (getAttr (ref "bucketArn") "region") "{Region}"))
                  (error
                   "Invalid configuration: region from ARN `{bucketArn#region}` does not match client region `{Region}` and UseArnRegion is `false`"))
                 (conditions
                  (assign "bucketPartition" (aws.partition (getAttr (ref "bucketArn") "region")))
                  (rules
                   (conditions
                    (assign "partitionResult" (aws.partition (ref "Region")))
                    (rules
                     (conditions
                      (stringEquals (getAttr (ref "bucketPartition") "name")
                                    (getAttr (ref "partitionResult") "name"))
                      (rules
                       (conditions
                        (isValidHostLabel (getAttr (ref "bucketArn") "region") #t)
                        (rules
                         (conditions
                          (isValidHostLabel (getAttr (ref "bucketArn") "accountId") #f)
                          (rules
                           (conditions
                            (assign "outpostType" (getAttr (ref "bucketArn") "resourceId[2]"))
                            (rules
                             (conditions
                              (assign "accessPointName" (getAttr (ref "bucketArn") "resourceId[3]"))
                              (rules
                               (conditions
                                (stringEquals (ref "outpostType") "accesspoint")
                                (rules
                                 (conditions
                                  (isSet (ref "Endpoint"))
                                  (assign "url" (parseURL (ref "Endpoint")))
                                  (endpoint
                                   "https://{accessPointName}-{bucketArn#accountId}.{outpostId}.{url#authority}"))
                                 (conditions
                                  (endpoint
                                   "https://{accessPointName}-{bucketArn#accountId}.{outpostId}.s3-outposts.{bucketArn#region}.{bucketPartition#dnsSuffix}"))))
                               (conditions
                                (error "Expected an outpost type `accesspoint`, found {outpostType}"))))
                             (conditions (error "Invalid ARN: expected an access point name"))))
                           (conditions (error "Invalid ARN: Expected a 4-component resource"))))
                         (conditions
                          (error
                           "Invalid ARN: The account id may only contain a-z, A-Z, 0-9 and `-`. Found: `{bucketArn#accountId}`"))))
                       (conditions
                        (error "Invalid region in ARN: `{bucketArn#region}` (invalid DNS name)"))))
                     (conditions
                      (error
                       "Client was configured for partition `{partitionResult#name}` but ARN (`{Bucket}`) has `{bucketPartition#name}`"))))))))
               (conditions
                (error
                 "Invalid ARN: The outpost Id may only contain a-z, A-Z, 0-9 and `-`. Found: `{outpostId}`"))))
             (conditions (error "Invalid ARN: The Outpost Id was not set"))))
           (conditions (error "Invalid ARN: Unrecognized format: {Bucket} (type: {arnType})"))))
         (conditions (error "Invalid ARN: No ARN type specified"))))
       (conditions (assign "arnPrefix" (substring (ref "Bucket") 0 4 #f))
                   (stringEquals (ref "arnPrefix") "arn:")
                   (not (isSet (aws.parseArn (ref "Bucket"))))
                   (error "Invalid ARN: `{Bucket}` was not a valid ARN"))
       (conditions (booleanEquals (ref "ForcePathStyle") #t)
                   (aws.parseArn (ref "Bucket"))
                   (error "Path-style addressing cannot be used with ARN buckets"))
       (conditions
        (assign "uri_encoded_bucket" (uriEncode (ref "Bucket")))
        (rules
         (conditions
          (assign "partitionResult" (aws.partition (ref "Region")))
          (rules
           (conditions
            (booleanEquals (ref "Accelerate") #f)
            (rules
             (conditions
              (booleanEquals (ref "UseDualStack") #t)
              (not (isSet (ref "Endpoint")))
              (booleanEquals (ref "UseFIPS") #t)
              (stringEquals (ref "Region") "aws-global")
              (endpoint
               "https://s3-fips.dualstack.us-east-1.{partitionResult#dnsSuffix}/{uri_encoded_bucket}"))
             (conditions
              (booleanEquals (ref "UseDualStack") #t)
              (not (isSet (ref "Endpoint")))
              (booleanEquals (ref "UseFIPS") #t)
              (not (stringEquals (ref "Region") "aws-global"))
              (booleanEquals (ref "UseGlobalEndpoint") #t)
              (rules
               (conditions
                (endpoint
                 "https://s3-fips.dualstack.{Region}.{partitionResult#dnsSuffix}/{uri_encoded_bucket}"))))
             (conditions
              (booleanEquals (ref "UseDualStack") #t)
              (not (isSet (ref "Endpoint")))
              (booleanEquals (ref "UseFIPS") #t)
              (not (stringEquals (ref "Region") "aws-global"))
              (booleanEquals (ref "UseGlobalEndpoint") #f)
              (endpoint
               "https://s3-fips.dualstack.{Region}.{partitionResult#dnsSuffix}/{uri_encoded_bucket}"))
             (conditions
              (booleanEquals (ref "UseDualStack") #f)
              (not (isSet (ref "Endpoint")))
              (booleanEquals (ref "UseFIPS") #t)
              (stringEquals (ref "Region") "aws-global")
              (endpoint "https://s3-fips.us-east-1.{partitionResult#dnsSuffix}/{uri_encoded_bucket}"))
             (conditions
              (booleanEquals (ref "UseDualStack") #f)
              (not (isSet (ref "Endpoint")))
              (booleanEquals (ref "UseFIPS") #t)
              (not (stringEquals (ref "Region") "aws-global"))
              (booleanEquals (ref "UseGlobalEndpoint") #t)
              (rules (conditions
                      (endpoint
                       "https://s3-fips.{Region}.{partitionResult#dnsSuffix}/{uri_encoded_bucket}"))))
             (conditions
              (booleanEquals (ref "UseDualStack") #f)
              (not (isSet (ref "Endpoint")))
              (booleanEquals (ref "UseFIPS") #t)
              (not (stringEquals (ref "Region") "aws-global"))
              (booleanEquals (ref "UseGlobalEndpoint") #f)
              (endpoint "https://s3-fips.{Region}.{partitionResult#dnsSuffix}/{uri_encoded_bucket}"))
             (conditions
              (booleanEquals (ref "UseDualStack") #t)
              (not (isSet (ref "Endpoint")))
              (booleanEquals (ref "UseFIPS") #f)
              (stringEquals (ref "Region") "aws-global")
              (endpoint
               "https://s3.dualstack.us-east-1.{partitionResult#dnsSuffix}/{uri_encoded_bucket}"))
             (conditions
              (booleanEquals (ref "UseDualStack") #t)
              (not (isSet (ref "Endpoint")))
              (booleanEquals (ref "UseFIPS") #f)
              (not (stringEquals (ref "Region") "aws-global"))
              (booleanEquals (ref "UseGlobalEndpoint") #t)
              (rules
               (conditions
                (endpoint
                 "https://s3.dualstack.{Region}.{partitionResult#dnsSuffix}/{uri_encoded_bucket}"))))
             (conditions
              (booleanEquals (ref "UseDualStack") #t)
              (not (isSet (ref "Endpoint")))
              (booleanEquals (ref "UseFIPS") #f)
              (not (stringEquals (ref "Region") "aws-global"))
              (booleanEquals (ref "UseGlobalEndpoint") #f)
              (endpoint
               "https://s3.dualstack.{Region}.{partitionResult#dnsSuffix}/{uri_encoded_bucket}"))
             (conditions
              (booleanEquals (ref "UseDualStack") #f)
              (isSet (ref "Endpoint"))
              (assign "url" (parseURL (ref "Endpoint")))
              (booleanEquals (ref "UseFIPS") #f)
              (stringEquals (ref "Region") "aws-global")
              (endpoint "{url#scheme}://{url#authority}{url#normalizedPath}{uri_encoded_bucket}"))
             (conditions
              (booleanEquals (ref "UseDualStack") #f)
              (isSet (ref "Endpoint"))
              (assign "url" (parseURL (ref "Endpoint")))
              (booleanEquals (ref "UseFIPS") #f)
              (not (stringEquals (ref "Region") "aws-global"))
              (booleanEquals (ref "UseGlobalEndpoint") #t)
              (rules
               (conditions
                (stringEquals (ref "Region") "us-east-1")
                (endpoint "{url#scheme}://{url#authority}{url#normalizedPath}{uri_encoded_bucket}"))
               (conditions
                (endpoint "{url#scheme}://{url#authority}{url#normalizedPath}{uri_encoded_bucket}"))))
             (conditions
              (booleanEquals (ref "UseDualStack") #f)
              (isSet (ref "Endpoint"))
              (assign "url" (parseURL (ref "Endpoint")))
              (booleanEquals (ref "UseFIPS") #f)
              (not (stringEquals (ref "Region") "aws-global"))
              (booleanEquals (ref "UseGlobalEndpoint") #f)
              (endpoint "{url#scheme}://{url#authority}{url#normalizedPath}{uri_encoded_bucket}"))
             (conditions (booleanEquals (ref "UseDualStack") #f)
                         (not (isSet (ref "Endpoint")))
                         (booleanEquals (ref "UseFIPS") #f)
                         (stringEquals (ref "Region") "aws-global")
                         (endpoint "https://s3.{partitionResult#dnsSuffix}/{uri_encoded_bucket}"))
             (conditions
              (booleanEquals (ref "UseDualStack") #f)
              (not (isSet (ref "Endpoint")))
              (booleanEquals (ref "UseFIPS") #f)
              (not (stringEquals (ref "Region") "aws-global"))
              (booleanEquals (ref "UseGlobalEndpoint") #t)
              (rules
               (conditions (stringEquals (ref "Region") "us-east-1")
                           (endpoint "https://s3.{partitionResult#dnsSuffix}/{uri_encoded_bucket}"))
               (conditions
                (endpoint "https://s3.{Region}.{partitionResult#dnsSuffix}/{uri_encoded_bucket}"))))
             (conditions
              (booleanEquals (ref "UseDualStack") #f)
              (not (isSet (ref "Endpoint")))
              (booleanEquals (ref "UseFIPS") #f)
              (not (stringEquals (ref "Region") "aws-global"))
              (booleanEquals (ref "UseGlobalEndpoint") #f)
              (endpoint "https://s3.{Region}.{partitionResult#dnsSuffix}/{uri_encoded_bucket}"))))
           (conditions (error "Path-style addressing cannot be used with S3 Accelerate"))))))))
     (conditions
      (isSet (ref "UseObjectLambdaEndpoint"))
      (booleanEquals (ref "UseObjectLambdaEndpoint") #t)
      (rules
       (conditions
        (assign "partitionResult" (aws.partition (ref "Region")))
        (rules
         (conditions
          (isValidHostLabel (ref "Region") #t)
          (rules (conditions (booleanEquals (ref "UseDualStack") #t)
                             (error "S3 Object Lambda does not support Dual-stack"))
                 (conditions (booleanEquals (ref "Accelerate") #t)
                             (error "S3 Object Lambda does not support S3 Accelerate"))
                 (conditions (isSet (ref "Endpoint"))
                             (assign "url" (parseURL (ref "Endpoint")))
                             (endpoint "{url#scheme}://{url#authority}{url#path}"))
                 (conditions
                  (booleanEquals (ref "UseFIPS") #t)
                  (endpoint "https://s3-object-lambda-fips.{Region}.{partitionResult#dnsSuffix}"))
                 (conditions
                  (endpoint "https://s3-object-lambda.{Region}.{partitionResult#dnsSuffix}"))))
         (conditions (error "Invalid region: region was not a valid DNS name."))))))
     (conditions
      (not (isSet (ref "Bucket")))
      (rules
       (conditions
        (assign "partitionResult" (aws.partition (ref "Region")))
        (rules
         (conditions
          (isValidHostLabel (ref "Region") #t)
          (rules
           (conditions (booleanEquals (ref "UseFIPS") #t)
                       (booleanEquals (ref "UseDualStack") #t)
                       (not (isSet (ref "Endpoint")))
                       (stringEquals (ref "Region") "aws-global")
                       (endpoint "https://s3-fips.dualstack.us-east-1.{partitionResult#dnsSuffix}"))
           (conditions
            (booleanEquals (ref "UseFIPS") #t)
            (booleanEquals (ref "UseDualStack") #t)
            (not (isSet (ref "Endpoint")))
            (not (stringEquals (ref "Region") "aws-global"))
            (booleanEquals (ref "UseGlobalEndpoint") #t)
            (rules (conditions
                    (endpoint "https://s3-fips.dualstack.{Region}.{partitionResult#dnsSuffix}"))))
           (conditions (booleanEquals (ref "UseFIPS") #t)
                       (booleanEquals (ref "UseDualStack") #t)
                       (not (isSet (ref "Endpoint")))
                       (not (stringEquals (ref "Region") "aws-global"))
                       (booleanEquals (ref "UseGlobalEndpoint") #f)
                       (endpoint "https://s3-fips.dualstack.{Region}.{partitionResult#dnsSuffix}"))
           (conditions (booleanEquals (ref "UseFIPS") #t)
                       (booleanEquals (ref "UseDualStack") #f)
                       (not (isSet (ref "Endpoint")))
                       (stringEquals (ref "Region") "aws-global")
                       (endpoint "https://s3-fips.us-east-1.{partitionResult#dnsSuffix}"))
           (conditions
            (booleanEquals (ref "UseFIPS") #t)
            (booleanEquals (ref "UseDualStack") #f)
            (not (isSet (ref "Endpoint")))
            (not (stringEquals (ref "Region") "aws-global"))
            (booleanEquals (ref "UseGlobalEndpoint") #t)
            (rules (conditions (endpoint "https://s3-fips.{Region}.{partitionResult#dnsSuffix}"))))
           (conditions (booleanEquals (ref "UseFIPS") #t)
                       (booleanEquals (ref "UseDualStack") #f)
                       (not (isSet (ref "Endpoint")))
                       (not (stringEquals (ref "Region") "aws-global"))
                       (booleanEquals (ref "UseGlobalEndpoint") #f)
                       (endpoint "https://s3-fips.{Region}.{partitionResult#dnsSuffix}"))
           (conditions (booleanEquals (ref "UseFIPS") #f)
                       (booleanEquals (ref "UseDualStack") #t)
                       (not (isSet (ref "Endpoint")))
                       (stringEquals (ref "Region") "aws-global")
                       (endpoint "https://s3.dualstack.us-east-1.{partitionResult#dnsSuffix}"))
           (conditions
            (booleanEquals (ref "UseFIPS") #f)
            (booleanEquals (ref "UseDualStack") #t)
            (not (isSet (ref "Endpoint")))
            (not (stringEquals (ref "Region") "aws-global"))
            (booleanEquals (ref "UseGlobalEndpoint") #t)
            (rules (conditions
                    (endpoint "https://s3.dualstack.{Region}.{partitionResult#dnsSuffix}"))))
           (conditions (booleanEquals (ref "UseFIPS") #f)
                       (booleanEquals (ref "UseDualStack") #t)
                       (not (isSet (ref "Endpoint")))
                       (not (stringEquals (ref "Region") "aws-global"))
                       (booleanEquals (ref "UseGlobalEndpoint") #f)
                       (endpoint "https://s3.dualstack.{Region}.{partitionResult#dnsSuffix}"))
           (conditions (booleanEquals (ref "UseFIPS") #f)
                       (booleanEquals (ref "UseDualStack") #f)
                       (isSet (ref "Endpoint"))
                       (assign "url" (parseURL (ref "Endpoint")))
                       (stringEquals (ref "Region") "aws-global")
                       (endpoint "{url#scheme}://{url#authority}{url#path}"))
           (conditions (booleanEquals (ref "UseFIPS") #f)
                       (booleanEquals (ref "UseDualStack") #f)
                       (isSet (ref "Endpoint"))
                       (assign "url" (parseURL (ref "Endpoint")))
                       (not (stringEquals (ref "Region") "aws-global"))
                       (booleanEquals (ref "UseGlobalEndpoint") #t)
                       (rules (conditions (stringEquals (ref "Region") "us-east-1")
                                          (endpoint "{url#scheme}://{url#authority}{url#path}"))
                              (conditions (endpoint "{url#scheme}://{url#authority}{url#path}"))))
           (conditions (booleanEquals (ref "UseFIPS") #f)
                       (booleanEquals (ref "UseDualStack") #f)
                       (isSet (ref "Endpoint"))
                       (assign "url" (parseURL (ref "Endpoint")))
                       (not (stringEquals (ref "Region") "aws-global"))
                       (booleanEquals (ref "UseGlobalEndpoint") #f)
                       (endpoint "{url#scheme}://{url#authority}{url#path}"))
           (conditions (booleanEquals (ref "UseFIPS") #f)
                       (booleanEquals (ref "UseDualStack") #f)
                       (not (isSet (ref "Endpoint")))
                       (stringEquals (ref "Region") "aws-global")
                       (endpoint "https://s3.{partitionResult#dnsSuffix}"))
           (conditions
            (booleanEquals (ref "UseFIPS") #f)
            (booleanEquals (ref "UseDualStack") #f)
            (not (isSet (ref "Endpoint")))
            (not (stringEquals (ref "Region") "aws-global"))
            (booleanEquals (ref "UseGlobalEndpoint") #t)
            (rules (conditions (stringEquals (ref "Region") "us-east-1")
                               (endpoint "https://s3.{partitionResult#dnsSuffix}"))
                   (conditions (endpoint "https://s3.{Region}.{partitionResult#dnsSuffix}"))))
           (conditions (booleanEquals (ref "UseFIPS") #f)
                       (booleanEquals (ref "UseDualStack") #f)
                       (not (isSet (ref "Endpoint")))
                       (not (stringEquals (ref "Region") "aws-global"))
                       (booleanEquals (ref "UseGlobalEndpoint") #f)
                       (endpoint "https://s3.{Region}.{partitionResult#dnsSuffix}"))))
         (conditions (error "Invalid region: region was not a valid DNS name."))))))))
   (conditions (error "A region must be set when sending requests to S3."))))
