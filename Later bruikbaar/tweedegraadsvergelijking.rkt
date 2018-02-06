#lang racket
(define (tweedegraads1 a b c)
  (define disc (sqrt(- (* b b) (* 4 a c))))
  (/ (- (- b) disc) (* 2 a)))
  
(define (tweedegraads2 a b c)
  (define disc (sqrt(- (* b b) (* 4 a c))))
  (/ (+ (- b) disc) (* 2 a)))

(define (geefposwortel a b c)
  (cond ((>= (tweedegraads1 a b c) 0) (tweedegraads1 a b c))
        ((>= (tweedegraads2 a b c) 0) (tweedegraads2 a b c))))