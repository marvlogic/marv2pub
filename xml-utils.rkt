#lang racket

(require xml)

(provide (all-defined-out))

(require racket/string)
(define (xml-root xml)
  (parameterize ((permissive-xexprs true))
    (xml-remstr (drop (xml->xexpr (document-element xml) ) 3))))

; remove all the strings from ls, used on top-level forms to strip
; out the whitespace elements
(define (string-blank? s)
  (and (string? s) (string=? "" (string-trim s))))

(define (xml-remstr ls)(filter (compose1 not string-blank?) ls))

; name??
; given the raw xml-xp it strips out the strings and header of the
; element, e.g.
; pick '(item () "   " (attr1 () val1 "  " ) (attr2 () val2 "  "))
; will return: '((attr1 () val1 "  ") ...)
; i.e. it leaves the inner items untouched.
(define (xml-pick xpr)(xml-remstr (drop xpr 2)))

(define (get-fields xpr) ( map car xpr ))

; look up the 'id' in the given xpr, if it exists then filter out all
; strings (for whitespace) and return the interesting part
(define (xml-get-structure id xpr)
  (define v (assoc id xpr))
  (if v (xml-remstr (drop v 2)) '(undef)))

; look up the id in the given xpr, which is assumed a list of attributes,
; and return the value. E.g. get-attr 'attr1 '((attr1 () "val1")(attr2 () "val3"))
; would return "val". Returns #f if id isn't found.
(define (xml-get-attr id xpr [default #f])
  (define v (assoc id xpr))
  (if v (third v) default))

;
(define (xml-get-list tag xml)
  ; TODO: - should check the required tag
  (if xml (map xml-pick xml) '()))

(define (hash->xexpr h
                     #:tag   [tag 'root]          ; default element name
                     #:key->sym [key->sym identity]) ; how to turn a key into a symbol
  ;; Helper: turn a hash entry into either an attribute pair or a child node.
  (define (entry->pair k v)
    (cond
      ;; Simple scalar → attribute
      [(or (string? v) (number? v) (boolean? v) (symbol? v))
       (list (key->sym k) (format "~a" v))]

      ;; Nested hash → child element (recursively)
      [(hash? v) (hash->xexpr v #:tag (key->sym k) #:key->sym key->sym)]

      ;; List → series of child elements (each item processed recursively)
      [(list? v)
       (map (lambda (item)
              (cond
                [(hash? item)
                 (list (key->sym k) (hash->xexpr item #:tag (key->sym k) #:key->sym key->sym))]
                [(or (string? item) (number? item) (boolean? item) (symbol? item))
                 (list (key->sym k) (format "~a" item))]
                [else
                 (error "Unsupported list item type:" item)]))
            v)]

      [else (error "Unsupported value type:" v)]))

  ;; Split entries into attributes vs. children
  (define attr-pairs '())
  (define child-nodes '())

  (for ([(k v) (in-hash h)])
    ;;(match-define (cons k v) kv)
    (define result (entry->pair k v))
    (cond
      [(and (pair? result) (symbol? (car result))) ; attribute
       (set! attr-pairs (cons result attr-pairs))]
      [(list? result)                               ; child node(s)
       (set! child-nodes (append child-nodes (if (list? (car result))
                                                 result
                                                 (list result))))]))
  ;; Build the final x‐expr: (tag (attr . val) ... child ...)
  (cons tag
        (append (reverse attr-pairs)   ; attributes first (order not significant)
                (reverse child-nodes)))
  )

(define hash->xmlstr (compose1 xexpr->string hash->xexpr))

;; (define cbc #hash((Bucket . happyrat-2212) (CreateBucketConfiguration . #hash((LocationConstraint . (eu-west-1
;;                                                                                                      eu-west-2))))))
;; (define xe (hash->xexpr #:tag 'root cbc))
;; xe
;; (xexpr->xml xe)
;; (xexpr->string xe)
