#lang racket

(provide
 hash-apply
 hash-filtern
 hash-filternc
 hash-merge<
 hash->flatlist
 expand-template
 compose1>
 system*/string
 mk-git-archive
 mk-ci-script)

(require
  racket/hash)

(define (hash-apply hs applyf)

  (define (inner k v)

    (define (sgl v)
      (cond [(hash? v) (hash-apply v applyf)]
            [(list? v) (map sgl v)]
            [else (applyf k v)]))
    (values k (sgl v)))

  (hash-map/copy hs inner))

(define/contract (hash->flatlist hs)
  (hash? . -> . (listof any/c))

  (define (foldit lst acc key-prefix)
    (define (flat kv acc)
      (cond [(hash? (cdr kv)) (foldit (hash->list (cdr kv)) acc (format "~a." (car kv)))]
            [(list? (cdr kv))
             (foldit (for/list ([ix (in-range (length (cdr kv)))]
                                [v (cdr kv)]) (cons (format "~a.~a" (car kv) ix) v)) acc key-prefix)]
            [else (cons (cons (string->symbol(format "~a~a" key-prefix (car kv))) (cdr kv)) acc)]))

    (foldl flat acc lst))
  (foldit (hash->list hs) '() ""))

(define (hash-match? hs matchf)

  (define (domatch h)
    (define (sgl k val)
      (or (matchf k val)
          (cond [(hash? val) (domatch val)]
                [(list? val) (ormap (lambda(v)(sgl k v)) val)]
                [else #f])))
    (for/or ([key (in-list(hash-keys h))])
      (sgl key (hash-ref h key))))

  (domatch hs))

(define (hash-filtern hs filt)

  (for/fold ([h (hash)])
            ([(k v) (in-hash hs)])
    (cond
      [(hash? v) (hash-set h k (hash-filtern v filt))]
      [(list? v) (hash-set h k (for/fold
                                ([lst '()])
                                ([i v] #:unless (filt k i))
                                 (cons i lst)))]
      [else (if (filt k v) (hash-set h k v) h)]
      )))


(define (hash-filternc hs filt)

  (define (handle-list k ls)
    (for/fold ([lst '()])
              ([i ls] #:when (filt k i))
      (cond
        [(list? i)
         (define nl (handle-list k i))
         (if (filt k nl) (cons nl lst) lst)]
        [(hash? i)
         (define nh (handle-hash i))
         (if (filt k nh) (cons nh lst) lst)]
        [else (if (filt k i) (cons i lst) lst)])))

  (define (handle-hash h)
    (for/fold ([hacc (hash)])
              ([(k v) (in-hash h)])
      (cond
        [(hash? v)
         (define nh (handle-hash v))
         (if (filt k nh) (hash-set hacc k nh) hacc)
         ]
        [(list? v)
         (define nl (handle-list k v))
         (if (filt k nl) (hash-set hacc k nl) hacc)]
        [else (if (filt k v) (hash-set hacc k v) hacc)]
        )))

  (handle-hash hs))

(define-syntax (compose1> stx)
  (syntax-case stx ()
    [(_ f ...)
     (with-syntax ([(rev ...) (reverse (syntax->list #'(f ...)))])
       #'(compose1 rev ...))]))

(define (hash-merge< l r)
  ;; merge the two hashes, if two keys exist in both then 'l' takes priority
  (hash-union l r #:combine/key (lambda(_k l _r)l)))


(define (expand-template str vars)

  (define var-spec #px"\\{[^{}]+\\}")

  (define (lookup v)
    (define name (substring v 1 (sub1 (string-length v))))
    (define path (string-split name "#"))

    (define (dive ks hs)
      (match ks
        [(list k) (hash-ref hs k (lambda()(raise (~a "can't find " k " in " hs))))]
        [(list-rest k rs) (dive rs (hash-ref hs k))]
        [else (raise (~a "unable to lookup " v))]))
    (dive path vars))

  (define split (regexp-match* var-spec str #:gap-select? #t))
  (string-join
   (for/list ([s split])
     (if (string-prefix? s "{") (lookup s) s))
   ""))

(define git (find-executable-path "git"))
(define tar (find-executable-path "tar"))

(define (system*/string . args)
  (define s (open-output-string))
  (and
   (parameterize ([current-output-port s])
     (apply system* args))
   (get-output-string s)))

(define (mk-git-archive file-name [gitref #f])
  (cond
    [(string? gitref)
     (apply system* git "archive" "--format=tar.gz" "-o " file-name gitref)]
    [(false? gitref)
     (define files (string-split(system*/string git "ls-files")))
     (apply system* tar "zcf" "archive.tgz" files)]
    [else (error "unknown gitref " gitref)]))

(define (mk-ci-script cmd)
  (format #<<EOF
#!/bin/bash
set -e
mkdir -p /project/aws/ec2
mkdir -p /project/aws/s3
cd /project
tar zxf /archive.tgz
cp /s3.json aws/s3/service-2.json
cp /ec2.json aws/ec2/service-2.json
~a
EOF
          cmd
          ))
