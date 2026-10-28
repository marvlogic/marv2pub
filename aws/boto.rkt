#lang racket

(provide
 api-request
 )

;;---------------------------------------------------------------------------------------------------
;; implementation

(require
  net/http-easy
  net/uri-codec
  marv/logger
  marv/xml-utils
  marv/resources
  marv/aws/boto-utils
  marv/aws/api-utils)

;; FIXME: This is ec2 specific, and should move.

;; FIXME: replace ec2-uri function  with endpoint rules (and generate them)

(define (api-request boto opid req-keb)
  ;; Sends an API request. we accept the request in kebab-case because the response (below) is parsed
  ;; into kebab-case on the return from the wire. But we still need to match up the request with the
  ;; shape, so that we can determine the locationName which is used for the request.

  ;; TODO: opid should also be kebab-case (will need to regenerate aws-ops.rkt and change gen-res.rkt)

  (define op (op-ref boto opid))
  (define input (shape-of (hash-ref op "input")) )
  (define req (mk-request-parts boto req-keb input))
  (define action-name (symbol->string opid))
  (define http (hash-ref op "http"))
  (define method (hash-ref http "method"))
  (define params (append (mk-headers (request-part-body req))
                         (list (cons 'Action action-name) (cons 'Version (api-version boto)))))
  (define form-body (alist->form-urlencoded params))

  ;; make the request to AWS
  (log-marv-debug (~a "form-body:" form-body))


  (define resp
    (case method
      [("POST")
       (define headers
         (date+authorize method
                         (ec2-uri boto)
                         (hash 'Content-Type "application/x-www-form-urlencoded")
                         (string->bytes/utf-8 form-body)))
       (post (ec2-uri boto) #:headers headers #:data form-body)]
      [("GET")
       (define uri (format "~a?~a" (ec2-uri boto) form-body))
       (define headers (date+authorize method uri (hash) (string->bytes/utf-8 "")))
       (get uri #:headers headers #:data form-body)]
      [else (raise "unsupported method")]))

  (log-marv-debug (~a "status:" (response-status-code resp)))
  (log-marv-debug (~a "body:" (response-body resp)))
  ;; TODO: - error handling
  (when (not (eq? 200 (response-status-code resp)))
    (raise-result-error 'api-request "200" (response-body resp)))

  (define xml-resp (response-xml resp))
  ;; root document, drops the headers
  (define xmlx (xml-root xml-resp))

  (define output (hash-ref (hash-ref op "output" (hash)) "shape" 'undef))
  (if (eq? output 'undef)
      ;; operations without an output *typically* have requestId:xxx and return:True
      (hash 'requestId (xml-get-attr 'requestId xmlx) 'return #t)
      (parse-resp boto output xmlx)))

(define (mk-headers hs)

  (define (handle-list l pfx acc)
    (for/fold ([a acc])
              ([v (in-list l)]
               [i (in-naturals)])
      (cond
        [(hash? v) (handle-hash v (format "~a~a." pfx i) a)]
        [(list? v) (handle-list v (format "~a~a." pfx i) a)]
        [(undef? v) a]
        [else (cons (cons (string->symbol (format "~a~a" pfx i)) (format "~a" v)) a)])))

  ;; Parameters in ec2 need to be captitalised.

  (define (capitalise raw)  (string-append (string-upcase (substring raw 0 1)) (substring raw 1)))

  (define (handle-hash h pfx acc)
    (for/fold ([a acc]) ([(klow v) (in-hash h)])
      (define kcap (capitalise klow))
      (cond
        [(hash? v) (handle-hash v (format "~a~a." pfx kcap) a)]
        [(list? v) (handle-list v (format "~a~a." pfx kcap) a)]
        [(undef? v) a]
        [else (cons (cons (string->symbol (format "~a~a" pfx kcap)) (format "~a" v)) a)])))

  (handle-hash hs "" '()))

