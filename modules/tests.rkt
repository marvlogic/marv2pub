#lang racket

(provide main)

(require
  marv/marvd
  marv/docker/driver
  marv/docker/resources
  marv/docker/barrier
  marv/state
  marv/stx-utils
  marv/utils
  )

(displayln "")
(displayln "********************************************************************************")

(define (main [purge? #f])
  (:= docker docker-driver)
  (:= marvd marv-driver)

  (install-docker-barriers)

  (:> image := docker :: docker-image
      name "marv-ci"
      build-path "docker-build/ci")

  (when (not purge?)

    (mk-git-archive "archive.tgz")

    (define (mk-script cmd)
      (mk-ci-script
       (string-append
        "raco pkg install --deps search-auto --batch -n marv /project\n"
        cmd)))

    (define unit-tests
      (hash
       "utils" "tests/unit/utils.rkt"
       "boto-utils" "tests/unit/boto-utils.rkt"
       "aws-ec2" "tests/unit/aws-ec2.rkt"
       "graph" "tests/unit/graph.rkt"))

    (define int-tests
      (hash
       "simple" "tests/int/simple.rkt"
       "basic" "tests/int/basic.rkt"
       "recovery" "tests/int/recovery.rkt"
       "loops" "tests/int/loops.rkt"
       ))

    (:= unit for/res
        ([(unit rkt) (in-hash unit-tests)]
         [idx (in-naturals)])
        (:> job := docker :: docker-job
            name (~a "marv-" unit)
            image-name (rref image name)
            environment (hash "MARV_TQ_DELAY" "0.1")
            input-files '(("archive.tgz" "/archive.tgz")
                          ("aws/ec2/service-2.json" "/ec2.json")
                          ("aws/s3/service-2.json" "/s3.json"))
            command (mk-script (~a "racket " rkt)))
        job
        )

    (::= unit-status marvd checkpoint
         (for/hash ([j unit])
           (displayln j)
           (values (rref j name) (rref j exit-status))))

    (:= int for/res
        ([(int rkt) (in-hash int-tests)]
         [idx (in-naturals)])
        (:> job := docker :: docker-job
            name (~a "marv-" int)
            deps (unit-status)
            image-name (rref image name)
            environment (hash "MARV_TQ_DELAY" "0.1")
            input-files '(("archive.tgz" "/archive.tgz")
                          ("aws/ec2/service-2.json" "/ec2.json")
                          ("aws/s3/service-2.json" "/s3.json"))
            command (mk-script (~a "racket " rkt)))
        job
        )

    ;; TODO: svars lifecycle don't update when tst is updated


    #t)
  )
