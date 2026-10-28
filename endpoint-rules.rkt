#lang racket

(provide
 rules
 conditions
 true
 false
 endpoint-run-rules
 (rename-out
  [is-set? isSet]
  [get-attr getAttr]
  [boolean-equals? booleanEquals]
  [string-equals? stringEquals]
  [aws-partition aws.partition]
  [parse-arn aws.parseArn]
  [is-virtual-hostable-s3-bucket? aws.isVirtualHostableS3Bucket]
  [parse-url parseURL]
  [uri-encode uriEncode]
  [is-valid-host-label? isValidHostLabel]
  [sub-string substring]
  [cerror error]
  )
 ref
 expand
 assign)

;; TODO: ipv6 matching
(require
  marv/logger
  marv/utils
  net/url-string
  (prefix-in _ net/uri-codec)
  (submod json for-extension)
  json)

;; (log-level-set 'warn)
(log-filter-set "(.*).rkt")

(define-syntax-rule (rules cs ...) (begin cs ...))
(define-syntax-rule (conditions e ... ) (and e ...))

(define (expand str) (expand-template str (params)))

(define (is-ipv4-addr? addr) (regexp-match? #px"(?:[0-9]{1,3}\\.){3}[0-9]{1,3}" addr))

(define (cerror m)
  (pretty-display (params))
  (raise m))

(define params (make-parameter (hash)))

(define (endpoint-run-rules ep-func default-params usr-params)
  (parameterize ([params (mk-endpoint-params default-params usr-params)])
    (expand (call/cc (lambda(k) (ep-func k))))))

(define (mk-endpoint-params defaults p)
  (for/hash ([(k v) (in-hash defaults)]
             #:when (or (hash-ref v "required") (hash-has-key? p k)))
    (values
     k
     (if (hash-has-key? p k)
         (hash-ref p k)
         (hash-ref v "default" #f)))))

(define (uri-encode s) (if (eq? 'None s) s (_uri-encode s)))

(define true #t)
(define false #f)
(define (is-set? p) (not (eq? 'None p)))
(define (ref p)
  (log-marv-debug (~a "ref " p (hash-ref (params) p 'None)))
  (hash-ref (params) p 'None))

(define (boolean-equals? x y)
  ;; (log-marv-debug (~a "boolean " x y))
  (when (or (not (boolean? x)) (not (boolean? y))) (raise 'bool-err))
  (eq? x y))

(define (string-equals? x y)
  (log-marv-debug (~a "string-equals " x " " y))
  (equal? x y))

(define (sub-string s start end from-end?)
  (define len (string-length s))
  (cond
    [(or (>= start end) (< len end)) 'None]
    [from-end? (substring s (- len end) (- len start))]
    [else (substring s start end)]))

(define (assign p v)
  (log-marv-debug (~a "assign " p v))
  (params (hash-set (params) p v)))

(define partitions
  (with-input-from-file "aws/partitions.json"
    (lambda ()
      (read-json* 'm (current-input-port)
                  #:replace-malformed-surrogate? #f
                  #:make-object make-immutable-hash
                  #:make-string values
                  #:null json-null #:make-list values #:make-key identity))))

(define (aws-partition region)
  (define parts (hash-ref partitions "partitions"))

  (define (is-match? region p)
    (or (regexp-match? (pregexp (hash-ref p "regionRegex")) region)
        (hash-has-key? (hash-ref p "regions") region)))

  (define (format-output p)
    (hash-set (hash-ref p "outputs") 'name (hash-ref p "id")))

  (cond
    [(eq? 'None region) (format-output (first parts))]
    [else
     (or
      (for/first ([p parts] #:when (is-match? region p)) (format-output p))
      (format-output (first parts)))]))


(define (parse-arn arn)

  (define (parse a)
    (match (string-split a ":")
      [(list-rest "arn" part svc region account resource)
       (hash 'partition part
             'service svc
             'region region
             'account account
             'resource (string-join resource ":"))]
      [_ #f]
      ))

  (match (parse arn)
    [(hash 'account accountId 'partition part 'service svc 'resource res #:open)
     (hash "accountId" accountId
           "partition" part
           "service" svc
           "resourceId" (string-split (string-replace res ":" "/") "/"))]
    [_else 'None])
  )

;; (parse-arn "arn:1:2:3:4:5:6")

;; TODO: get-attr needs to support path lookups such as x.y[1]
;; (see utils/expand-template)

(define (get-attr val path)
  (log-marv-debug (~a "get-attr " val ":" path))
  (cond
    [(hash? val) (hash-ref val path)]
    [(eq? 'None val) ''None]))

(define (parse-url urlstr)

  (define (normalise-path ps)
    (string-append
     (string-join
      (reverse
       (for/fold ([acc null])
                 ([p (map path/param-path ps)])
         (case p
           [(up) (cdr acc)]
           [("" same) acc]
           [else (cons p acc)])))
      "/")
     "/"))

  (define (path->str ps)
    (string-join
     (for/list ([p (map path/param-path ps)]
                #:unless (equal? "" p))
       (case p
         [(up) ".."]
         [(same) "."]
         [else p]))
     "/"))

  (define (netloc h p) (if p (format "~a:~a" h p) h))

  (match (string->url urlstr)
    [(url (and scheme (or "http" "https")) _ host port _ path #f _)
     (hash "scheme" scheme
           "authority" (netloc host port)
           "path" (path->str path)
           "normalizedPath" (normalise-path path)
           "isIp" (is-ipv4-addr? host)
           )]
    [_ 'None]
    ))

(define (is-valid-host-label? label allow-subdomains?)
  (log-marv-debug (~a "is-valid-host? " label))
  (define host-label #px"^(?!-)[a-zA-Z\\d-]{1,63}(?<!-)$")
  (cond
    [(eq? 'None label) #f]
    [allow-subdomains?
     (for/and ([s (string-split label ".")]) (is-valid-host-label? s #f))]
    [else (regexp-match? host-label label)]))

(define (is-virtual-hostable-s3-bucket? val allow-subdomains?)
  (log-marv-debug (~a "is-virtual? " val allow-subdomains?))
  (cond
    [(or (eq? 'None val) (< (string-length val) 3)
         (not (equal? (string-downcase val) val))
         (is-ipv4-addr? val))
     #f]
    [else (is-valid-host-label? val allow-subdomains?)]
    ))


