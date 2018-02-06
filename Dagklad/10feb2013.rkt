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

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
          guess
          (sqrt-iter (improve guess x)
                     x)))



#|(define (good-enough? guess x)
   (> (/ guess (abs(- guess (oud guess x)))) 1000000000))

(define (oud guess x)
  (geefposwortel 1 (* -2 guess) x guess x))

(define (tweedegraads1 a b c)
  (define disc (sqrt(- (* b b) (* 4 a c))))
  (/ (+ (- b) disc) (* 2 a)))
  
(define (tweedegraads2 a b c)
  (define disc (sqrt(- (* b b) (* 4 a c))))
  (/ (- (- b) disc) (* 2 a)))

(define (geefposwortel a b c guess x)
  (cond ((and(>= (tweedegraads1 a b c) 0)(= guess (/ (+ (tweedegraads1 a b c) (/ x (tweedegraads1 a b c))) 2))) (tweedegraads1 a b c))
        ((and(>= (tweedegraads2 a b c) 0)(= guess (/ (+ (tweedegraads2 a b c) (/ x (tweedegraads2 a b c))) 2))) (tweedegraads2 a b c))))|#




  









                 




