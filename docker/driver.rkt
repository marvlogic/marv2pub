#lang racket

(provide docker-driver)

(require
  marv/stx-utils
  marv/model
  marv/resources
  marv/logger
  marv/docker/resources)

(define dockerd
  (mk-res-driver
   'docker
   (lambda(lc-cmd)

     ;; FIXME: exception leaves resource in state; doesn't clean up (same for aws driver)
     (log-marv-debug (~a "Looking up " (op-cmd-type lc-cmd)))
     (define hdlr (hash-ref types-lookup (op-cmd-type lc-cmd)
                            (lambda()
                              (raise
                               (~a "Docker type handler not found:" lc-cmd
                                   "in " (hash-keys types-lookup ))))))
     (log-marv-debug (~a "lc-handler " hdlr ))
     (define result (hdlr lc-cmd))
     (log-marv-debug (~a "result " result))
     (if (op-diff? lc-cmd)
         (if (op-diff-result? result) result (op-diff-result (hash) #f))
         result)
     )))

(define docker-driver (lambda(id)(driver (res-driver-id dockerd) id (res-driver-exec dockerd) null)))
