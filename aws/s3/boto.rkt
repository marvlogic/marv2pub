#lang racket

(provide
 api-request
 get-shape-kw-proc
 model-attributes-mutability
 mk-blueprint
 mk-computation-shape
 )

(require
  racket/runtime-path
  xml
  marv/logger
  (prefix-in _ "boto-s3.rkt")
  (prefix-in _ marv/aws/boto-utils))

(define-runtime-path service-file "service-2.json")
(define boto
  (hash-set
   (with-input-from-file service-file (lambda () (_read-json-string-keys)))
   'service-uri "https://s3.eu-west-1.amazonaws.com/"))

(define api-request ((curry _api-request) boto))
(define get-shape-kw-proc ((curry _get-shape-kw-proc) boto))
(define model-attributes-mutability ((curry _model-attributes-mutability) boto))
(define mk-computation-shape ((curry _mk-computation-shape) boto))
(define mk-blueprint ((curry _mk-blueprint) boto))

