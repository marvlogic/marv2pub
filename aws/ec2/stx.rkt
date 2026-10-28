#lang racket

(provide ec2-api)

;; ec2-api wraps most of the AWS EC2 Api requests into resources that can be stored in a state file.
;; For direct access to the API, it is best to use the aws-ec2-shapes and aws-ec2-ops modules
;; directly.

(require
  racket/hash
  (for-syntax syntax/parse)
  marv/aws/ec2/shapes
  marv/aws/ec2/ops
  (only-in marv/resources
           mk-resource
           computation-hs
           resource-cfg)
  marv/state)

; TODO: create a blueprint, can we be certain that overlaying
; a request on top of an 'empty' response object is valid,
; i.e. that the response object doesn't 'require' any fields?

(define (merge-blueprint req resp)
  (hash-union req resp
              #:combine/key (lambda(k req _)req)))


(define (safe-state-ref id) (if (state-in? id) (computation-hs (resource-cfg (state-ref id))) 'undef))

(define-syntax (ec2-api stx)
  (define (mk-cmd ctx prefix type suffix)
    (datum->syntax ctx (string->symbol
                        (format "~a~a~a" prefix (syntax->datum type) suffix))))
  (syntax-case stx (create create0 delete describe)
    [(_ create0 type cfg ...)
     (with-syntax
         ([cmd-req (mk-cmd #'cmd "create-" #'type "-request")]
          [cmd-res (mk-cmd #'cmd  "" #'type "")]
          [cmd-full (mk-cmd #'cmd "create-" #'type "")])
       #'(curry (lambda(_ drv id)
                  (mk-resource
                   'cmd-full id drv
                   (merge-blueprint
                    (cmd-req cfg ...)
                    (mk-computation-shape 'cmd-res
                                          (lambda()(safe-state-ref id)))))))) ]
    [(_ create type cfg ...)
     (with-syntax
         ([cmd-req (mk-cmd #'cmd "create-" #'type "-request")]
          [cmd-res (mk-cmd #'cmd "create-" #'type "-result")]
          [cmd-full (mk-cmd #'cmd "create-" #'type "")])
       #'(curry (lambda(_ drv id)
                  (mk-resource
                   'cmd-full id drv
                   (merge-blueprint
                    (cmd-req cfg ...)
                    (mk-computation-shape 'cmd-res
                                          (lambda()(safe-state-ref id)))))))) ]

    [(_ describe type cfg ...)
     (with-syntax
         ([cmd-req (mk-cmd #'cmd "describe-" #'type "-request")]
          [cmd-res (mk-cmd #'cmd "describe-" #'type "-result")]
          [cmd-full (mk-cmd #'cmd "describe-" #'type "")])
       #'(curry (lambda(_ drv id)
                  (mk-resource
                   'cmd-full id drv
                   (merge-blueprint
                    (cmd-req cfg ...)
                    (mk-computation-shape 'cmd-res
                                          (lambda()(safe-state-ref id)))))))) ]

    [(_ delete type cfg ...)
     (with-syntax
         ([cmd-req (mk-cmd #'cmd "delete-" #'type "-request")]
          [cmd-full (mk-cmd #'cmd "delete-" #'type "")])
       #'( curry (lambda(_ drv id)
                   (mk-resource 'cmd-full id drv (cmd-req cfg ...))) )) ]

    ;; [(_ any-op cfg ...)
    ;;  (with-syntax
    ;;      ([cmd-req (mk-cmd #'cmd any-op)]
    ;;       [cmd-full (mk-cmd #'cmd "delete-" #'type "")])
    ;;    #'( curry (lambda(_ drv id)
    ;;                (mk-resource 'cmd-full id drv (cmd-req cfg ...))) )) ]

    ))

