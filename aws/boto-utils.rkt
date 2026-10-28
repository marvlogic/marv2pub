#lang racket

(provide
 (struct-out request-part)
 (all-defined-out))

(require
  casemate
  json
  (submod json for-extension)
  net/http-easy
  racket/hash
  marv/resources
  marv/model
  marv/xml-utils
  marv/logger)

(define ->boto-key (compose1 ->PascalCase symbol->string))
(define ->marv-key (compose1 string->symbol ->kebab-case))

(define (op-ref boto opid) (hash-ref (hash-ref boto "operations") (->boto-key opid)))
(define (ec2-uri boto) (hash-ref boto "service-uri"))
(define (api-version boto) (hash-ref (hash-ref boto "metadata") "apiVersion"))
(define (shape-ref boto shpid) (hash-ref (hash-ref boto "shapes") shpid))
(define (shape-of m) (hash-ref m "shape"))
(define (type-of s) ( hash-ref s "type" ))
(define (members-of shp) (hash-ref shp "members"))

(define (read-json-string-keys)
  (read-json* 'm (current-input-port)
              #:replace-malformed-surrogate? #f
              #:make-object make-immutable-hash
              #:make-string values
              #:null json-null #:make-list values #:make-key identity))


(define get-shape-kw-proc
  (make-keyword-procedure
   (lambda (kws kw-args . args)
     (define boto (car args))
     (define shape-id (symbol->string (cadr args)))
     (define shape (shape-ref boto shape-id))
     (define type (type-of shape))
     (case type
       [("structure")
        ;; Need to convert keywords->symbols in this hash so we can
        ;; match lookups in the members
        (define kwh
          (for/hash ([k kws]
                     [ka kw-args])
            (values ((compose1 ->PascalCase keyword->string) k) ka)))
        (define members (members-of shape))

        (define required (hash-ref shape "required" '()))
        (let ([diff (set-subtract required (hash-keys kwh))])
          (unless (null? diff)
            (raise (~a "structure " shape-id " missing required field(s): " diff))))

        ;; Output will be a hash of member-name(string)->value
        (for/hash ([(m v) (in-hash members)])
          (log-marv-debug (format "~v(~a) = ~v" m (hash-ref v "shape") (hash-ref kwh m #f)))

          (let* ([value (hash-ref kwh m 'undef)]
                 [locn (->marv-key m)])
            (values locn value)))]
       ;(log-marv-debug (format "list-of: ~v" (hash-ref (hash-ref shape "member") "shape")))
       ;; TODO: - implement template?
       [("list") (cdr args)]
       ;[("string") (car kw-args)]
       [else (raise (format "mks: unsupported type ~a for shape ~a" type shape-id))])
     )))

(define (mk-computation-shape boto shape-id rootfn)
  ;; makes a shape from the given shape-id (kebab-case) which is populated by
  ;; computation functions. Each member of the hash will one of:
  ;;
  ;; - a zero-parameter lambda function returning a primitive (string, int etc)
  ;; - a nested hash, populated by further computations
  ;; - a list with one element of the item shape, and inside this element will be computations.
  ;;
  ;; Returns a hash containing the computation functions

  ;; NOTE: might consider merging a populated shape, where values are
  ;; already provided

  (define (safe-hash-ref hs k) (if (undef? hs) hs (hash-ref hs k 'undef)))
  (define (safe-car ls) (if (undef? ls) 'undef (car ls)))

  (define (mkcs shid read-val-fn)
    (define shape (shape-ref boto shid))
    (define type (type-of shape))
    (case type
      [("structure")
       (define members (members-of shape))

       ;; Output will be a hash of member-name(string)->value
       (for/hash ([(m v) (in-hash members)])
         (values (->marv-key m)
                 (mkcs
                  (shape-of v)
                  (lambda()(safe-hash-ref (read-val-fn) (->marv-key m)))))) ]
      [("list")
       (log-marv-debug (format "list-of: ~v" (hash-ref (hash-ref shape "member") "shape")))
       ;; putting an exemplar item into a list allows a user to build a
       ;; query on the assumption that something like a 'describe' request will
       ;; return at least one item.
       (define mber (hash-ref shape "member"))
       (define item-shape (shape-of mber))
       (list (mkcs item-shape (lambda()(safe-car (read-val-fn)))))]
      [("long" "integer" "string" "boolean") read-val-fn]
      [("timestamp") read-val-fn]
      [else (raise (format "unsupported computation type ~a for shape ~a" type shid))]))

  (mkcs (->boto-key shape-id) rootfn))

(define (mk-blueprint boto shape-id [undef 'undef])
  ;; public function so shape-id is kebab-case

  (define (mkbp shid)
    (define shape (shape-ref boto shid))
    (define type (type-of shape))
    (case type
      [("structure")
       (define members (members-of shape))

       ;; Output will be a hash of member-name(string)->value
       (for/hash ([(m v) (in-hash members)])
         ;(log-marv-debug (format "~v(~a) = ~v" m (hash-ref v "shape") (hash-ref pc-hash m #f)))
         (values (->marv-key m) (mkbp (shape-of v) )))]
      ;(log-marv-debug (format "list-of: ~v" (hash-ref (hash-ref shape "member") "shape")))
      [("list")
       (define mber (hash-ref shape "member"))
       (define item-shape (shape-of mber))
       (list (mkbp item-shape))]
      [("boolean" "string" "integer" "long" "timestamp" "blob") (cons undef (string->symbol type))]
      [else (raise (format "mk-blueprint: unsupported type ~a for shape ~a" type shid))]))

  (mkbp (->boto-key shape-id)))

(define (model-attributes-mutability boto create-shp-id [desc-shp-id #f] [mod-shp-id #f])

  ;; works out which attributes of a model for a shape are immutable, mutable or read-only. This is
  ;; based on set operations performed against the given three shapes (create, describe, modify).
  ;; If mod-shp-id is #f (default) then the whole shape is immutable.

  (define (lookup shp-id) ( members-of (shape-ref boto (->boto-key shp-id))))

  (define create-attrs (hash-keys(lookup create-shp-id)))
  (define desc-attrs (if desc-shp-id (hash-keys(lookup desc-shp-id)) '()))

  (define-values (mod-reqd-attrs mutable-attrs)
    (cond
      [mod-shp-id
       (define reqd (hash-ref (shape-ref boto (->boto-key mod-shp-id)) "required"))
       (values reqd (set-subtract (hash-keys(lookup mod-shp-id)) reqd))]
      [else (values '() '())]))

  (define immutable-attrs (set-subtract create-attrs mutable-attrs))
  (define ro-attrs (set-union mod-reqd-attrs (set-subtract desc-attrs (set-union create-attrs mutable-attrs))))

  ;; hash-union will explode if there are any overlapping attributes; this is desirable as it will
  ;; expose models that don't fit our assumptions and hence can't use this functionality.

  (define model
    (hash-union
     (for/hash ([k immutable-attrs]) (values (->marv-key k) 'immutable))
     (for/hash ([k mutable-attrs]) (values (->marv-key k) 'mutable))
     (for/hash ([k ro-attrs]) (values (->marv-key k) 'read-only))
     ))

  (hash->model-attrs model))


(define (serialise-shape boto shape-id body)

  ;; Assumes that body is already in camelCase format, and has been separated using mk-request-parts

  (define (is-attr? spec)
    (define namespace? (hash-ref spec "xmlNamespace" #f))
    (if namespace?
        (list 'xmlns (hash-ref namespace? "uri"))
        (hash-has-key? spec "xmlAttribute")))

  (define (atom id attrs mems) (append (list (string->symbol id) attrs) mems))

  (define (mkxml shid req)
    (log-marv-debug (~a "looking up " shid))
    (define shape (shape-ref boto shid))
    (define type (type-of shape))
    (case type
      [("structure")
       (define members (members-of shape))
       (for/fold ([acc-mems '()]
                  [acc-attrs '()]
                  #:result (values acc-attrs acc-mems))
                 ([(m v) (in-hash members)]
                  #:when (hash-has-key? req m))
         (log-marv-debug (~a "structure field " m))
         (define location (string->symbol (hash-ref v "locationName" m)))
         (define attr? (is-attr? v))
         (define namespace? (list? attr?))
         (log-marv-debug (~a "attr status " attr?))
         (cond
           [namespace?
            (define-values (_ mems) (mkxml (shape-of v) (hash-ref req m)))
            (values (cons (atom m (list attr?) mems) acc-mems) acc-attrs)]
           [attr?
            (define-values (_ mems) (mkxml (shape-of v) (hash-ref req m)))
            (values acc-mems (cons (cons location mems) acc-attrs))]
           [else
            (define-values (attrs mems) (mkxml (shape-of v) (hash-ref req m)))
            (values (cons (atom m attrs mems) acc-mems) acc-attrs)]))
       ]
      ;(log-marv-debug (format "list-of: ~v" (hash-ref (hash-ref shape "member") "shape")))
      [("list")
       (define mber (hash-ref shape "member"))
       (define item-shape (shape-of mber))
       (list (mkxml item-shape req))]
      [("boolean" "string" "integer" "long" "timestamp") (values '() (list req))]
      [else (raise (format "unsupported type ~a for shape ~a" type shid))]))

  ;; TODO: what if body doesn't have the initial shape-id member?
  (define-values (attrs mems)(mkxml shape-id body))
  (atom shape-id attrs mems))

(define (serialise-payload boto shape-id body)
  (define payload (hash-ref (shape-ref boto shape-id) "payload"))
  (define serialised (serialise-shape boto shape-id body))
  (define (is-payload? x) (equal? x (string->symbol payload)))
  (for/first ([at (cddr serialised)])
    (match at
      [(list-rest (? is-payload? _) _ _) at]
      [_  #f])))


(struct request-part (headers uri query body) #:transparent)

(define (mk-request-parts boto req shape-id)

  ;; Creates all the components necessary based on the kebab-case request. shape-id is pascal-case
  ;; string because it's internal to the API module

  ;; as we walk the members of the shape, and all it's sub-shapes etc, we need to partition the
  ;; parameters into uri, header, query and body parameters. (body-params are defined below). Each
  ;; member may have a 'location' field to indicate where the parameter goes; if it's not uri, header,
  ;; headers or querystring then we assume it's the body. Request member keys are converted from
  ;; kebab-case to camelCase.

  (define uri-params (make-hash))
  (define header-params (make-hash))
  (define query-params (make-hash))

  (define (walker req shape-id)
    (define shape (shape-ref boto shape-id))
    (define type (type-of shape))
    (case type
      [("structure")

       ;; create a pascal-case hash of the request, that we can use for matching between the boto
       ;; shape members and the request

       ;; (define pc-req
       ;;   (for/hash ([(k v) (in-hash req)]
       ;;              #:unless (undef? v))
       ;;     (values (->boto-key k) v)))

       (define members (members-of shape))

       ;; check if any required fields have been missed

       (define required (map ->marv-key (hash-ref shape "required" '())))
       (let ([diff (set-subtract required (hash-keys req))])
         (unless (null? diff)
           (raise (~a "structure " shape-id " missing required field(s): " diff))))

       (define body-params (make-hash))
       (for ([(m v) (in-hash members)]
             #:when (not (undef? (hash-ref req (->marv-key m) 'undef))))
         (define mkey (->marv-key m))
         (log-marv-debug (format "~v(~a) = ~v" m (hash-ref v "shape") (hash-ref req mkey #f)))
         (define location (hash-ref v "location" 'body))
         (log-marv-debug location)
         (let* ([value (hash-ref req mkey)]
                [locn (hash-ref v "locationName" m)])
           (define tgt
             (case location
               [("uri") uri-params]
               [("header" "headers") header-params]
               [("querystring") query-params]
               [else body-params]))
           (hash-set! tgt locn (walker value (shape-of v) ))))
       body-params]

      [("list")
       (define mber (hash-ref shape "member"))
       (define item-shape (shape-of mber))
       (map (lambda (i) (walker i item-shape)) req) ]

      [("boolean") (if req "true" "false")]
      [("string" "timestamp") req]
      [("integer" "long") req]
      [else (raise (format "unsupported type ~a for shape ~a" type shape-id))]))

  (request-part header-params uri-params query-params (walker req shape-id)))

(define (parse-response-parts boto shape-id read-resp-header read-resp-xml-body)

  ;; parses a response into a hash structure, given by the 'shape-id'. The response data can be found
  ;; in HTTP headers and/or an XML response in the body. The boto structure is walked to determine
  ;; where to get the data from. If it's a header, it calls read-resp-header to get it. If not, and
  ;; read-resp-xml-body has not been called, it is called to read the body and then the resulting
  ;; xexpr is parsed into a hash. This hash is then consulted for all subsequent body members.

  ;; as well as the boto spec and shape-id, the function requires two passing in two functions:

  ;; read-resp-header is a function that when called with a header-id (symbol, lower-case) returns the
  ;; value for the header.
  ;;
  ;; read-resp-xml-body is a function that returns the xexpr of the body contained in a response.
  ;; (should only be called once)

  (define (walker shape-id)
    (define shape (shape-ref boto shape-id))
    (define type (type-of shape))
    (case type
      [("structure")

       (define members (members-of shape))

       ;; body-reply will be populated by response data the first time we encounter a member with
       ;; location=body
       (define body-reply #f)
       (define (lookup-response-body ref)
         (when (not body-reply)
           (set! body-reply (parse-resp boto shape-id (read-resp-xml-body)))
           (log-marv-debug (~a "set body:" body-reply)))
         (hash-ref body-reply ref 'undef))

       (for/hash ([(m v) (in-hash members)])
         (define location (hash-ref v "location" 'body))
         (define locn (->marv-key (hash-ref v "locationName" m)))
         (log-marv-debug (~a "member:" m " locationName:" locn " location:" location ))
         (define val
           (case location
             [("header") (read-resp-header locn)]
             [else (lookup-response-body (->marv-key m))]))
         (values (->marv-key m) val))
       ]
      ;; all S3 response (*.Output) appear to be structures
      [else (raise (format "unsupported parse-response-parts ~a for shape ~a" type shape-id))]))
  (walker shape-id))

(define (parse-resp boto shape-id xml)
  ;; parse the incoming xml request from the wire protocol. hashes returned
  ;; will have kebab-case keys

  (log-marv-debug  (format "parsing shape ~a from xml:~v" shape-id xml))
  (cond

    ;; WARN: this null test fixed issue with clientToken being 'null' in an
    ;; instance-response but that probably happened because we strip whitespace;
    ;; if clientToken was actually "" then handling that is probably the
    ;; underlying bug that needs fixing.

    [(null? xml) 'undef]
    [(and (pair? xml) (undef? (car xml))) 'undef]
    [else
     (define shape (shape-ref boto shape-id))
     (define type (type-of shape))
     (case type
       [("structure")
        (define members (members-of shape))
        (for/hash ([m (hash-keys members)])
          (define v (hash-ref members m))
          (define location (string->symbol (hash-ref v "locationName" m)))
          (log-marv-debug (format "struct member: ~v (shape ~a) in location ~v" m (hash-ref v "shape") location))
          ;(log-marv-debug  (format "checking for ~v in xml:\n~a" location xml ))
          ;(log-marv-debug  (format "->> ~a" (xml-get-structure location xml)))
          (values (->marv-key m)
                  (parse-resp boto
                              (shape-of v)
                              (xml-get-structure location xml))))]
       [("list")
        (define mber (hash-ref shape "member"))
        (define item-shape (hash-ref mber "shape"))
        (define location (hash-ref mber "locationName" item-shape))
        (log-marv-debug (format "list-of: ~v in ~v" item-shape location))
        (log-marv-debug  (format "get: ~a" (xml-get-list 'item xml) ))
        (map (lambda (i) (parse-resp boto item-shape i)) (xml-get-list 'item xml))]
       [("boolean")
        (case (car xml)
          [("false") #f]
          [("true") #t]
          [else (raise (format "~v is not boolean" (car xml)))])]
       [("string" "timestamp") (format "~a" (car xml))]
       [("integer" "long") (string->number (car xml))]
       [else (raise (format "unsupported type ~a for shape ~a, xml=~a" type shape-id xml))])]))

