#lang racket

(provide aws-driver)

(require
  marv/resources
  marv/logger
  marv/aws/types
  aws/keys
  )

; TODO: hard-coded creds
(aws-cli-profile "happyrat")
(credentials-from-file!)

(define lcdriver
  (mk-res-driver
   'aws-ec2-lc
   (lambda(lc-cmd)
     (define hdlr (hash-ref types-lookup (op-cmd-type lc-cmd)
                            (lambda()(raise (~a "Type handler not found:" lc-cmd)))))
     (log-marv-debug (~a "lc-handler " hdlr ))
     (define result (hdlr lc-cmd))
     (log-marv-debug (~a "result " result))
     (if (op-diff? lc-cmd)
         (if (op-diff-result? result) result (op-diff-result (hash) #f))
         result)
     )))

(define aws-driver (lambda(id)(driver (res-driver-id lcdriver) id (res-driver-exec lcdriver) null)))
