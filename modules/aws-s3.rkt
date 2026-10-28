#lang racket

(provide main)

(require
  marv/aws/driver
  marv/aws/s3/resources
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
    (:> bucket := aws :: aws-s3-bucket name "marv-2210" region "eu-west-1")

    #t)
  )


