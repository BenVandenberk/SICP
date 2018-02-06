#!r6rs
(import (rnrs lists (6))
        (rnrs base (6))
        (rnrs io simple (6)))

(define (square x)(* x x))

(define (average x y)
           (/ (+ x y) 2))

#|(define (improve guess x)
          (average guess (/ x guess)))

(define (sqrt2 x) (sqrt-iter 1.0 10 x))

(define (good-enough? guess oudguess)
   (> (/ guess (abs(- guess oudguess))) 1000000))

(define (sqrt-iter guess oudguess x)
  (if (good-enough? guess oudguess)
      guess
      (sqrt-iter (improve guess x) guess x)))|#

(define (sqrt2 x)
  (define (improve guess)
    (average guess (/ x guess)))
  (define (good-enough? guess oudguess)
    (> (/ guess (abs(- guess oudguess))) 1000000))
  (define (sqrt-iter guess oudguess)
    (if (good-enough? guess oudguess) guess
        (sqrt-iter (improve guess) guess)))
  (sqrt-iter 1.0 10))

#|  Merk op dat in een block structure de embedded definitions EERST komen, dan pas het
'uitvoerende deel' [i.c. (sqrt-iter 1.0 10)]  |#
   
