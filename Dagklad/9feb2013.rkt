#!r6rs
(import (rnrs lists (6))
        (rnrs base (6))
        (rnrs io simple (6)))

(define (square x)(* x x))

(define (average x y)
           (/ (+ x y) 2))

(define (improve guess x)
          (average guess (/ x guess)))

(define (sqrt2 x) (sqrt-iter 1.0 x))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))

(define (sqrt-iter guess x)
  (new-if (good-enough? guess x)
          guess
          (sqrt-iter (improve guess x)
                     x)))





                 




