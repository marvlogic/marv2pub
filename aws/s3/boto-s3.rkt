#lang racket

(provide
 api-request
 (struct-out request-part)
 )

;;---------------------------------------------------------------------------------------------------
;; implementation

(require
  net/http-easy
  xml
  marv/logger
  marv/xml-utils
  marv/aws/boto-utils
  marv/endpoint-rules
  marv/aws/api-utils
  marv/aws/s3/endpoint-rules)

(define endpoint-rule-set (with-input-from-file "aws/s3/endpoint-rule-set-1.json" (lambda () (read-json-string-keys))))
(define endpoint-defaults (hash-ref endpoint-rule-set "parameters"))

;; TODO: this is copy-pasta from the ec2 module, so has a lot of redundant code to remove.

;; Update 2025-10-02 - Boto does a lot of per-API customisation, might be ok to leave this code
;; specific to AWS S3 instead of trying to generalise it

(define (api-request boto opid req-keb)

  ;; Implementation based on the RestXMLSerializer class from botocore/serializers

  ;; Sends an API request. we accept the request in kebab-case because the response (below) is parsed
  ;; into kebab-case on the return from the wire. But we still need to match up the request with the
  ;; shape, so that we can determine the locationName which is used for the request.

  ;; TODO: opid should also be kebab-case (will need to regenerate aws-ops.rkt and change gen-res.rkt)

  (define op (op-ref boto opid))
  (define input (shape-of (hash-ref op "input")) )
  (define req (mk-request-parts boto req-keb input))
  (define http (hash-ref op "http"))
  (define expect-code (hash-ref http "responseCode" 200))
  (define method (hash-ref http "method"))

  ;; TODO: we don't use requestUri, but if we did it would need extra handling as per
  ;; botocore/handlers.py (BUILTIN_HANDLERS)

  ;; (define request-uri (hash-ref http "requestUri"))

  (log-marv-debug (~v "request-parts:" req))

  ;; TODO: need to follow boto's lead as per botocore/regions.py:539 _get_provider_params i.e.
  ;; static/contextParam from the member specification, to avoid the hard-coded values

  ;; FIXME: all the hard-coded vals, and date+authorize-g
  (define ep-params (hash "Region" "eu-west-1" "Bucket" (hash-ref (request-part-uri req) "Bucket" 'None)))
  (define endpoint (endpoint-run-rules s3-endpoint-rules endpoint-defaults ep-params))

  (define resp
    (case method
      [("GET")
       (define uri (format "~a/" endpoint))
       (log-marv-debug (~a "GET " uri))
       (define headers (date+authorize-g "s3" "eu-west-1" method uri (hash) (string->bytes/utf-8 "")))
       (get uri #:headers headers)]
      [("PUT")
       (define uri (format "~a/" endpoint))
       (define body (xexpr->string (serialise-payload boto input (request-part-body req))))
       (log-marv-debug (~a "PUT " uri " " body))
       (define headers (date+authorize-g "s3" "eu-west-1" method uri (hash) (string->bytes/utf-8 body)))
       (put uri #:headers headers #:data body)]
      [("DELETE")
       (define uri (format "~a/" endpoint))
       (log-marv-debug (~a "DELETE " uri))
       (define headers (date+authorize-g "s3" "eu-west-1" method uri (hash) (string->bytes/utf-8 "")))
       (delete uri #:headers headers)]
      [else (raise "unsupported method")]))

  (log-marv-debug (~a "status:" (response-status-code resp)))
  (log-marv-debug (~a "body:" (response-body resp)))
  (log-marv-debug (~a "headers:" (response-headers resp)))
  ;; TODO: - error handling
  (when (not (eq? expect-code (response-status-code resp)))
    (raise-result-error 'api-request (~a expect-code) (response-body resp)))

  (define output (hash-ref (hash-ref op "output" (hash)) "shape" 'undef))

  ;; (define xml-resp (response-xml resp))
  ;; root document, drops the headers
  ;; (define xmlx (xml-root xml-resp))

  (if (eq? output 'undef)
      ;; operations without an output *typically* have requestId:xxx and return:True
      ;; (hash 'requestId #;(xml-get-attr 'requestId xmlx) 'return #t)
      (hash 'return #t)
      (parse-response-parts boto output
                            (lambda(r)(response-headers-ref resp r))
                            (lambda()(xml-root (response-xml resp))))))

