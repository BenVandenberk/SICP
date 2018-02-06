#lang racket

(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
      (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (average x y)
  (/ (+ x y) 2))

(define (repeated f n)
  (define (helper f g n)
    (if (= n 0)
        g
        (helper f (compose f g) (- n 1))))
  (helper f (lambda (x) x) n))

(define (compose f g)
  (lambda (x) (f (g x))))

(define (square x) (* x x))

#|///////////////////////////////////////////////////|#

(define (cube-root x)
  (fixed-point (average-damp (lambda (y) (/ x (square y))))
               1.0))


(define (average-damp-times f times)
  ((repeated average-damp times) f))


(define (nth-root x n)
  (fixed-point (average-damp-times (lambda (y) (/ x (expt y (- n 1)))) (roundup(2log n)))
               1.0))

(define (2log x)
  (/ (log x) (log 2)))

(define (roundup x)
  (define (roundup-iter x y)
    (if (< x 1)
        (+ 1 (- y x))
        (roundup-iter (- x 1) y)))
  (roundup-iter x x))

