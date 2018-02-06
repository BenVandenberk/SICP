#!r6rs
(import (rnrs lists (6))
        (rnrs base (6))
        (rnrs io simple (6)))

#|                  Formule:   (x/y^2 + 2y)/3                 |#

(define (square x) (* x x))

#|(define (cube-root-iter guess preguess x)
  (if (good-enough? guess preguess)
      guess
      (cube-root-iter (improve guess x) guess x)))

(define (improve guess x)
  (/ (+ (* 2.0 guess) (/ x (square guess))) 3.0))

(define (good-enough? guess preguess)
  (< (* guess (abs(- guess preguess))) 0.0001))|#

(define (cube-root x)
  (define (good-enough? guess preguess)
    (< (* guess (abs(- guess preguess))) 0.0001))
  (define (improve guess)
    (/ (+ (* 2.0 guess) (/ x (square guess))) 3.0))
  (define (cube-root-iter guess preguess)
    (if (good-enough? guess preguess) guess
        (cube-root-iter (improve guess) guess)))
  (cube-root-iter 1.0 0.0))

#|  Merk op dat in een block structure de embedded definitions EERST komen, dan pas het
'uitvoerende deel' [i.c. (cube-root-iter 1.0 0.0)]  |#

