#lang racket

(define (fib n)
  (fib-iter 1 0 0 1 n))
(define (fib-iter a b p q count)
  (define (paccent)(+ (square p) (square q)))
  (define (qaccent)(+ (square q) (* 2 p q)))
  (define (square x)(* x x))
  (cond ((= count 0) b)
        ((even? count)
         (fib-iter a
                   b
                   (paccent)     ; compute p'
                   (qaccent)     ; compute q'
                   (/ count 2)))
        (else (fib-iter (+ (* b q) (* a q) (* a p))
                        (+ (* b p) (* a q))
                        p
                        q
                        (- count 1)))))

