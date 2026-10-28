#lang racket

(require
  marv/gen-barriers
  marv/resources
  marv/barrier
  "ec2/utils.rkt"
  )

(define (aws-instance-state-barrier state-str)
  (lambda(bcfg contn)
    (wait-for-state-barrier
     (lambda(cfg)(equal? state-str (hash-ref (hash-ref cfg 'state) 'name))) bcfg contn)))

(define (aws-igw-detach-barrier)
  (define (chk igw)
    (= 0 (length (vpc-instances-state (hash-ref igw 'vpc-id)))))
  (lambda(bcfg contn) (wait-for-state-barrier chk bcfg contn)))

(define (barrier-match-pre bcfg)
  (match bcfg
    [(barrier-cfg _ 'delete (resource 'aws-igw-attachment _ _ _)) (aws-igw-detach-barrier)]
    [_ #f]))

(define (barrier-match-post bcfg)
  (match bcfg
    [(barrier-cfg _ 'create (resource 'aws-instance _ _ _)) (aws-instance-state-barrier "running")]
    [(barrier-cfg _ 'delete (resource 'aws-instance _ _ _)) (aws-instance-state-barrier "terminated")]
    [_ #f]))

(barrier-set 'pre barrier-match-pre)
(barrier-set 'post barrier-match-post)

