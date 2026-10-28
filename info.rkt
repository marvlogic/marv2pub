#lang info
(define collection "marv")
(define deps
  '(
    ;;"graph-lib"
    ;; "http-easy-lib"
    ;; "rackunit-lib"
    ;; "sha"
    ;; "brag"
    ;; "uri-template"
    ;; "aws-sigv4"
    "aws"
    "base"
    "debug"
    "casemate"
    "graph"
    "http-easy"
    "http"
    "lens"
    "http-easy-lib"
    "racket-langserver"
    "remote-shell-lib"
    ))
;; (define build-deps '("scribble-lib" "racket-doc" "rackunit-lib"))
;; (define scribblings '(("scribblings/marv.scrbl" ())))
(define pkg-desc "Description Here")
(define version "0.1")
(define pkg-authors '(Kevin Haines))
(define license '(Apache-2.0 OR MIT))
(define racket-launcher-names '("marv"))
(define racket-launcher-libraries '("command"))

