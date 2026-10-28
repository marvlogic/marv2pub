#lang racket

(provide log-level-set
         log-filter-set
         log-marv-debug
         log-marv-info
         log-marv-warn
         log-marv-error
         log-marv-fatal)

(define levels #(OFF FATAL ERROR WARN INFO DEBUG))

(define log-level (make-parameter 2))
(define log-filter (make-parameter (regexp ".*")))

(define (log-filter-set re-str) (log-filter (regexp re-str)))
(define (log-level-set lvl)
  (log-level
   (case lvl
     [(debug) 5]
     [(info) 4]
     [(warn) 3]
     [(error) 2]
     [(fatal) 1]
     )))

(define (log-if? level src line msg)
  (when (and (<= level (log-level)) (regexp-match? (log-filter) src))
    (displayln (format "\033[1m~a ~a:~a\033[0m ~a" (vector-ref levels level) src line msg))))

(define-syntax (log-marv stx)
  (syntax-case stx ()
    ([_ level msg]
     (with-syntax
         ([line (syntax-line stx)]
          [src (let-values
                   ([(_ p t) ; grab the filename component
                     (split-path(syntax-source stx))]) p)])
       #'(log-if? level src line msg)))))

(define-syntax-rule (log-marv-debug m) (log-marv 5 m))
(define-syntax-rule (log-marv-info m) (log-marv 4 m))
(define-syntax-rule (log-marv-warn m) (log-marv 3 m))
(define-syntax-rule (log-marv-error m) (log-marv 2 m))
(define-syntax-rule (log-marv-fatal m) (log-marv 1 m))
