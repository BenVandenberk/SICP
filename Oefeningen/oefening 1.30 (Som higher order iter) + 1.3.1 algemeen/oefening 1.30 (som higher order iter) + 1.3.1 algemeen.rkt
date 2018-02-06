#lang racket

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

#|/////////////////////////////////////////////////////////////////|#

(define (inc n) (+ n 1))

(define (cube x) (* x x x))

(define (sum-cubes a b)
  (sum cube a inc b))

#|/////////////////////////////////////////////////////////////////|#

(define (identity x) x)

(define (sum-integers a b)
  (sum identity a inc b))

#|/////////////////////////////////////////////////////////////////|#

(define (pi-sum a b)
  (define (pi-term x)
    (/ 1.0 (* x (+ x 2))))
  (define (pi-next x)
    (+ x 4))
  (sum pi-term a pi-next b))

#|   Oefening 1.30   |#

(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ result (term a)))))
  (iter a 0))
