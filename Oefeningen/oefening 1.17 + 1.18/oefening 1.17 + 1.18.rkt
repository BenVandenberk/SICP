#lang racket

#|  Oefening 1.17 ( O(log n), RECURSIEF   |#

(define (halve x) (/ x 2))
(define (double x) (* x 2))

(define (maal a b)
  (cond ((= a 1) b)
        ((even? a) (maal (halve a)(double b)))
        (else (+ b (maal (- a 1) b)))))

#|  Oefening 1.18 (idem 1.17, maar ITERATIEF)   |#

(define (maal-it a b)
  (define (maal-it-iter a b x)
    (cond ((= a 1)(+ b x))
          ((even? a)(maal-it-iter (halve a) (double b) x))
          (else (maal-it-iter (- a 1) b (+ x b)))))
  (maal-it-iter a b 0))