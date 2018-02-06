#lang racket

(define (cont-frac n d k)
  (define (iter n d k stop)
    (if (> k stop)
        0
        (/ (n k)
           (- (d k) (iter n d (+ k 1) stop)))))
  (iter n d 1 k))

(define (square x) (* x x))

(define (tan-cf x k)
  (cont-frac (lambda (k) (if (= k 1) x
                             (square x)))
             (lambda (k) (+ -1 (* 2 k)))
             k))


