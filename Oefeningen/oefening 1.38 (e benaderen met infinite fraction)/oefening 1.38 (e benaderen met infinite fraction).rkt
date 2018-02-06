#lang racket

(define (deelbaar? x y)
  (= (remainder x y) 0))

(define (euler x)
  (if (deelbaar? (+ x 1) 3)
      (* (/ (+ x 1) 3) 2)
      1))

(define (cont-frac n d k)
  (define (iter n d k stop)
    (if (> k stop)
        1
        (/ (n k)
           (+ (d k) (iter n d (+ k 1) stop)))))
  (iter n d 1 k))

(define (cont-frac-it* n d k)
  (define (iter n d k deler)
    (let ((next (+ (d (- k 1))
                   (/ (n k)
                      deler))))
      (if (= k 1)
          (/ (n 1) deler)
          (iter n d (- k 1) next))))
  (iter n d k (d k)))