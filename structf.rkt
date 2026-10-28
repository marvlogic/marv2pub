#lang racket

(provide struct/f struct/mo)

(require (for-syntax racket/syntax syntax/parse))

(define-syntax (struct/f stx)
  (syntax-case stx ()
    [(_ struct-id (field ...))
     (with-syntax*
         ([acc-name (format-id #'struct-id "~a##field" #'struct-id)]
          [pred-id (format-id #'struct-id "~a?" #'struct-id)]
          [fld-cnt (length (syntax->list #'(field ...)))]
          [((field-ids i) ...)
           (for/list ([f (syntax->list #'(field ...))]
                      [i (in-naturals)])
             (list (format-id #'struct-id "~a.~a" #'struct-id f) i))]
          )
       #`(begin
           (define-values(_ struct-id pred-id acc-name mutname) (make-struct-type 'struct-id #f fld-cnt 0 #f null #f))
           (define (field-ids s) (acc-name s i)) ...
           ))]
    ))

(define-syntax (struct/kw stx)

  (define syntax->keyword (compose1 string->keyword symbol->string syntax-e))

  (define-syntax-class typedfield
    (pattern (fid:id type:id)
      #:with kw-arg #`(#,(syntax->keyword #'fid) fid)))

  (syntax-parse stx
    [(_ struct-id (field:typedfield ...))
     (with-syntax*
         ([acc-name (format-id #'struct-id "~a##field" #'struct-id)]
          [pred-id (format-id #'struct-id "~a?" #'struct-id)]
          [constr-id (format-id #'struct-id "_~a" #'struct-id)]
          [kwconstr-id (format-id #'struct-id "kw-~a" #'struct-id)]
          [fld-cnt (length (syntax->list #'(field.fid ...)))]
          [((ctor-arg ...) ...) #`(field.kw-arg ...)]
          [((field-ids i) ...)
           (for/list ([f (syntax->list #'(field.fid ...))]
                      [i (in-naturals)])
             (list (format-id #'struct-id "~a.~a" #'struct-id f) i ))]
          )
       #`(begin
           (define-values(_ constr-id pred-id acc-name mutname) (make-struct-type 'struct-id #f fld-cnt 0 #f null #f))
           ;(define (field-accs s) (acc-name s 0)) ...
           (define (field-ids s) (acc-name s i)) ...
           (define/contract (struct-id field.fid ...)(-> field.type ... pred-id )(constr-id field.fid ...))
           (define (kwconstr-id ctor-arg ... ...) (struct-id field.fid ...))
           ))]
    ))

(define-syntax (struct/kwopt stx)

  (define syntax->keyword (compose1 string->keyword symbol->string syntax-e))

  (define-syntax-class typedfield
    (pattern (fid:id type:id)
      #:with kw-arg #`(#,(syntax->keyword #'fid) fid)
      #:with kw-ctr #'type)
    (pattern (fid:id type:id 'opt)
      #:with kw-arg #`(#,(syntax->keyword #'fid) [fid (void)])
      #:with kw-ctr #'(or/c void? type)))

  (syntax-parse stx
    [(_ struct-id (field:typedfield ...))
     (with-syntax*
         ([acc-name (format-id #'struct-id "~a##field" #'struct-id)]
          [pred-id (format-id #'struct-id "~a?" #'struct-id)]
          [constr-id (format-id #'struct-id "_~a" #'struct-id)]
          [kwconstr-id (format-id #'struct-id "kw-~a" #'struct-id)]
          [fld-cnt (length (syntax->list #'(field.fid ...)))]
          [((ctor-arg ...) ...) #`(field.kw-arg ...)]
          [((field-ids i) ...)
           (for/list ([f (syntax->list #'(field.fid ...))]
                      [i (in-naturals)])
             (list (format-id #'struct-id "~a.~a" #'struct-id f) i ))]
          )
       #`(begin
           (define-values(_ constr-id pred-id acc-name mutname) (make-struct-type 'struct-id #f fld-cnt 0 #f null #f))
           (define (field-ids s) (acc-name s i)) ...
           (define/contract (struct-id field.fid ...)(-> field.kw-ctr ... pred-id )(constr-id field.fid ...))
           (define (kwconstr-id ctor-arg ... ...) (struct-id field.fid ...))
           ))]
    ))


; variant allows mandatory and optional structure fields to be declared as two separate
; lists. 

; (struct/om my-id (mandatory-fields ...) (optional-fields ...))

; Each field is in the form '(field-id type-pred). The keyword constructor is
; given the identifier 'struct-id'

(define-syntax (struct/mo stx)

  (define syntax->keyword (compose1 string->keyword symbol->string syntax-e))

  (define-syntax-class mandatory-field
    (pattern (fid:id type:id)
      #:with kw-arg #`(#,(syntax->keyword #'fid) fid)
      #:with kw-ctr #`(#,(syntax->keyword #'fid) type)))

  (define-syntax-class optional-field
    (pattern (fid:id type:id)
      #:with kw-arg #`(#,(syntax->keyword #'fid) [fid 'undef])
      #:with kw-ctr #`(#,(syntax->keyword #'fid) type)))

  (syntax-parse stx
    [(_ struct-id (mnd-field:mandatory-field ...) (opt-field:optional-field ...))
     (with-syntax
         ([acc-name (format-id #'struct-id "~a#field" #'struct-id)]
          [pred-id (format-id #'struct-id "~a?" #'struct-id)]
          [constr-id (format-id #'struct-id "_~a" #'struct-id)]
          [fields-id (format-id #'struct-id "~a#fields" #'struct-id)]

          ; not sure if we need the introspection field anymore. If we do then
          ; consider adding the fields-id syntax directly to the list instead
          ; of the add1's

          [fld-cnt ( add1 (length (syntax->list #'(mnd-field.fid ... opt-field.fid ...))) )]
          
          ; wrapping these ((x ...) ...) allows us to un-nest a level in the template
          [((mnd-fld-arg ...) ...) #'(mnd-field.kw-arg ...)]
          [((opt-field-arg ...) ...) #'(opt-field.kw-arg ...)]
          [((mand-kw ...) ...) #'(mnd-field.kw-ctr ...)]
          [((opt-kw ...) ...) #'(opt-field.kw-ctr ...)]

          [((field-ids i) ...)
           (for/list ([f (syntax->list #'(mnd-field.fid ... opt-field.fid ...))]
                      [i (in-naturals)])
             (list (format-id #'struct-id "~a.~a" #'struct-id f) (add1 i) ))]
          )
       #'(begin
           (define-values(_ constr-id pred-id acc-name mutname) (make-struct-type 'struct-id #f fld-cnt 0 #f null #f))
           (define fields-id (list 'mnd-field.fid ... 'opt-field.fid ...))
           (define (field-ids s) (acc-name s i)) ...
           (define/contract
             (struct-id mnd-fld-arg ... ... opt-field-arg ... ...)
             (->* (mand-kw ... ...) (opt-kw ... ...) pred-id )
             (constr-id fields-id mnd-field.fid ... opt-field.fid ...))
           ))]
    ))
