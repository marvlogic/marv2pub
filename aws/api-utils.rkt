#lang racket

(provide date+authorize
         date+authorize-g)

(require
  racket/struct
  net/url
  aws/sigv4
  aws/keys
  http/request
  (only-in marv/resources undef?)
  )

;; (define ec2-endpoint (make-parameter (endpoint "ec2.eu-west-1.amazonaws.com" #t)))
;; (define ec2-region (make-parameter "eu-west-1"))
;; (define service "ec2")

(define/contract (date+authorize method uri heads body)
  (-> string? string? dict? bytes? dict?)
  (define dt (seconds->gmt-8601-string 'basic (current-seconds)))
  (define ep-host (url-host (string->url uri)))
  (define ep-host-parts (string-split ep-host "."))
  (define service (first ep-host-parts))
  (define region (second ep-host-parts))
  (define body-hash (sha256-hex-string body))
  (let ([heads (dict-set* heads 'Host ep-host 'Date dt 'x-amz-content-sha256 body-hash)])
    (add-v4-auth-heads #:heads heads
                       #:method method
                       #:uri uri
                       #:sha256 body-hash
                       #:region region
                       #:service service)))


(define/contract (date+authorize-g service region method uri heads body)
  ;; This method differs from date+authorize by requiring service and region to be passed in, rather
  ;; than extracted from the uri
  (-> string? string? string? string? dict? bytes? dict?)
  (define dt (seconds->gmt-8601-string 'basic (current-seconds)))
  (define ep-host (url-host (string->url uri)))
  (define body-hash (sha256-hex-string body))
  (let ([heads (dict-set* heads 'Host ep-host 'Date dt 'x-amz-content-sha256 body-hash)])
    (add-v4-auth-heads #:heads heads
                       #:method method
                       #:uri uri
                       #:sha256 body-hash
                       #:region region
                       #:service service)))

(define (mk-req-params req-struct-fields req-struct params-map)

  ;; creates a list of pairs for use as form parameters, where each pair is the name/value. Value is
  ;; taken from the each of the structure fields.

  ;;   req-struct-fields   field names of the structure, in struct-order
  ;;   req-struct          the structure holding the values
  ;;   params-map          map from field-name -> member-or-location-name
  ;;
  ;; undefined fields are left out of the resulting list.

  (for/list ([fld-name req-struct-fields]
             [fld-val (struct->list req-struct)]
             #:unless (undef? fld-val))
    (let ([n (hash-ref params-map fld-name)])
      (cond
        [(list? fld-val)
         (for/list ([(f idx) (in-indexed fld-val)])
           (cons (format "~a.~a" n (add1 idx)) 'list))]
        [(struct? fld-val)
         (let ([struct-fields (vector-ref (struct->vector fld-val) 1)])
           (cons n (list? struct-fields)))]
        [else (cons n fld-val)]))))
