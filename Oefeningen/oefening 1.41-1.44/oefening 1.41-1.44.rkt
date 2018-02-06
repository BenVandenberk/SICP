#lang racket

(define (double f)
  (lambda (x) (f (f x))))

(define (inc x) (+ x 1))

(define (compose f g)
  (lambda (x) (f (g x))))

(define (square x) (* x x))

(define (repeated f n)
  (define (helper f g n)
    (if (= n 0)
        g
        (helper f (compose f g) (- n 1))))
  (helper f (lambda (x) x) n))

#| De oplossing van Bill the Lizard was recursief: |#

(define (repeated* f n)
  (if (= n 1)
      f
      (compose f (repeated* f (- n 1)))))

(define (smooth f)
  (let ((dx 0.7))
    (lambda (x) (/ (+ (f x)
                      (f (- x dx))
                      (f (+ x dx)))
                   3))))

(define (n-fold-smoothed f n)
  ((repeated smooth n) f))