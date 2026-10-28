#lang racket

(require
  racket/match
  marv/aws/boto-utils)

(define (handle-args as)
  (for/list ([a as])
    (match a
      [(hash 'ref varid) (list 'ref varid)]
      [(hash 'fn fnid 'argv args)
       (cons (string->symbol fnid) (handle-args args))]
      #;[_ (error (~v a))]
      [_ a])))


;; NOTE: might yet need ->kebab-case for the variables, depending on interfaces...

(define (handle-condition c)
  (match c
    [(hash 'fn fnid 'argv args)
     (cons (string->symbol fnid) (handle-args args))]
    [(hash 'fn fnid 'argv args 'assign asgn)
     `(assign ,asgn ,(cons (string->symbol fnid) (handle-args args)))]
    [_ (error (~v c))]
    ))

(define (handle-rule r)
  (define cs (hash-ref r 'conditions))
  (define type (hash-ref r 'type))
  (define final
    (case type
      [("tree") (handle-rules (hash-ref r 'rules))]
      [("error") (list 'error (hash-ref r 'error))]
      [("endpoint") (list 'endpoint (hash-ref (hash-ref r 'endpoint) 'url))]))
  `(conditions
    ,@(for/list ([c cs]) (handle-condition c))
    ,final))

(define (handle-rules rs)
  `(rules ,@(for/list ([r rs]) (handle-rule r))))

(define (generate)
  (displayln "#lang racket")
  (displayln "(require marv/endpoint-rules)")
  (displayln "(define (log x) (displayln x)#t)")
  (displayln "(define (epr endpoint)")
  (print (handle-rules endpoint-rules))
  (displayln ")")
  (displayln "(call/cc (lambda(k) (epr k)))")
  )

(define endpoint-rule-set (with-input-from-file "aws/s3/endpoint-rule-set-1.json" (lambda () (read-json-string-keys))))
(define endpoint-rules (hash-ref endpoint-rule-set "rules"))
(define endpoint-params (hash-ref endpoint-rule-set "parameters"))

(generate)
