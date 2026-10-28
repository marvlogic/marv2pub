#lang racket

(require json)
(require graph)
(require casemate)
(require lens)

(define boto
  (with-input-from-file "service-2.json"
    (lambda()(read-json))))

(define maxResults (hash-ref-nested-lens 'maxResults))
(define MaxResults (hash-ref-nested-lens 'MaxResults))

;; (define dvar-shape (hash-ref-nested-lens 'DescribeVpcEndpointAssociationsRequest 'members 'MaxResults 'shape))
;; (define shapes
;;   ; Can't remove with a lense...
;;   (hash-remove
;;    (let ((shp (hash-ref boto 'shapes)))
;;      (lens-set/list shp
;;                     MaxResults (lens-view maxResults shp)
;;                     dvar-shape "MaxResults")) 'maxResults))

(define shapes (hash-ref boto 'shapes))

(with-output-to-file #:exists 'replace "shapes.rkt"
  (lambda()
    (displayln #<<EOF
#lang racket
(require "boto.rkt")
(provide (all-defined-out))

EOF
               )
    (for ([k (hash-keys shapes #t)])
      (displayln (format "(define ~a (curry get-shape-kw-proc '~a))" (->kebab-case k) k)))
    ))

(define ops (hash-ref boto 'operations))

(with-output-to-file #:exists 'replace "ops.rkt"
  (lambda()
    (displayln #<<EOF
#lang racket
(require "boto.rkt")
(provide (all-defined-out))
EOF
               )
    (for ([k (hash-keys ops #t)])
      (displayln (format "(define (~a r) (api-request '~a r))" (->kebab-case k) k)))
    ))

