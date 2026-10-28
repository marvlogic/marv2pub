#lang racket

(require
  rackunit
  rackunit/text-ui
  marv/utils)

(define tsh
  (hash "Bucket" "happyrat-2210"
        "Region" "eu-west-1"
        "scheme" "https"
        "partitionResult" (hash "dnsSuffix" "amazonaws.com")))

(exit
 (run-tests
  (test-suite
   "utils"
   (test-case
    "expanders"
    (check-equal? (expand-template "xyz{scheme}xyz" tsh) "xyzhttpsxyz")
    (check-equal? (expand-template "{scheme}xyz" tsh) "httpsxyz")
    (check-equal? (expand-template "{scheme}" tsh) "https")
    (check-equal? (expand-template "{scheme}.{Region}" tsh) "https.eu-west-1")
    (check-equal? (expand-template "xyz{scheme}" tsh) "xyzhttps")
    (check-equal? (expand-template "xyz" tsh) "xyz")
    (check-equal? (expand-template "{scheme}://{Bucket}.s3.{Region}.{partitionResult#dnsSuffix}/root" tsh)
                  "https://happyrat-2210.s3.eu-west-1.amazonaws.com/root")
    ))))
