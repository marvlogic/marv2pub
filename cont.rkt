#lang racket

(define (retry-operation max-attempts)
  (call/cc
   (lambda (success)
     (for ([attempt '(1 2 3 4 5)])
       (if (> attempt max-attempts)
           "Max attempts reached"
           (begin
             (displayln (format "Attempt ~a" attempt))
             (if (zero? (random 4)) ; Simulating success or failure
                 (success "Operation succeeded")
                 #f))))
     success)))

(define (probe max-attempts)
  (call/cc
   (lambda (success)
     (displayln 'here)
     success)))

;; (define p (probe 1))
;; (p 2)

(define defer (call/cc (lambda (k) (k 2))))
(+ 1 (+ 1 (call/cc (lambda(k)(k 2)))))
;; (let ([p (retry-operation 5)])
;;   (when (procedure? p) (p 1 2)))

; queue of zero-argument functions that invoke continuations.
; forks for each value.
; tries the next search branch in the queue.
; Not necessarily one of the branches introduced by this call
(define (choice vals)
  (call-with-current-continuation
   (lambda (k)
     (set! search-branches (append search-branches
                                   (for/list ([val vals])
                                     (lambda () (k val)))))
     (when (empty? search-branches)
       (error "search failed"))
     (define next (first search-branches))
     (set! search-branches (rest search-branches))
     (next))))
(define (assert condition)
  (if condition
      (void)
      ; (choice (list)) kills this search branch
      (choice (list))))

(define search-branches (list))
(define (find-pythag)
  (define a (choice (list 1 2 3 4 5 6 7 8 9 10 11 12 13)))
  (define b (choice (list 1 2 3 4 5 6 7 8 9 10 11 12 13)))
  (define c (choice (list 1 2 3 4 5 6 7 8 9 10 11 12 13)))
  (assert (<= a b))
  (assert (equal? (+ (* a a) (* b b))
                  (* c c)))
  (displayln (list a b c))
  (displayln (list a b c)))

(find-pythag)


(define mut (make-parameter 0))
(define (mut1) (mut (add1 (mut)))(mut))

(define suspended (make-hash))
(define (resume key computation)
  (displayln "resumer")
  (define future
    (call/cc
     (lambda(k)
       (hash-set!
        suspended key
        (lambda(fut)
          #;(hash-remove! suspended key)
          (k fut)))
       )))
  (mut1)
  (displayln 'recalled)
  (displayln (mut))

  ;; This is void because it is the result of hash-set! above
  (if (void? future)
      'pending
      (computation future)))


(resume 'id1 ~a)
(resume 'id2 ~a)
(resume 'id3 ~a)
suspended
(mut)
((hash-ref suspended 'id1) 5)

(define (block-until key exec pred)
  (displayln "blocker")
  (define future
    (call/cc
     (lambda(k)
       (displayln 'in-call-cc)
       ; storing in the hash is essential to be able to recover the continuation;
       ; if you try to store the returned lambda in a variable, then it can only
       ; get used once. If not stored here then it must be handled outside
       (hash-set! suspended key (lambda(fut) (k fut))))))
  (displayln 'checking)
  (define res (exec))
  (displayln res)
  (displayln (pred res))
  (if (void? future)
      'pending
      (cond
        [(pred res)
         (displayln 'finished)
         (displayln future)
         (hash-remove! suspended key)
         future]
        [else
         (displayln (pred res))
         'pending])))

(define (status-check val) (lambda(c)(eq? val c)))
(mut 0)
(block-until 'id4 mut1 (status-check 3))
((hash-ref suspended 'id4) 'wibbly)
suspended
(mut)


(define (yield-until exec pred)
  (call/cc
   (lambda (k)
     (if (pred (exec))
         'done
         (lambda () (k 'resumed))))))

(define f (yield-until mut1 (status-check 2)))
f
(f)
(mut 0)
