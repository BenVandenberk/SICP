#lang racket

#|   RECURSIEF   |#

(define (cont-frac n d k)
  (define (iter n d k stop)
    (if (> k stop)
        1
        (/ (n k)
           (+ (d k) (iter n d (+ k 1) stop)))))
  (iter n d 1 k))

#|   ITERATIEF   |#

(define (cont-frac-it n d k)
  (define (iter n d k deler)
    (define (next x)
      (+ (d (- k 1))
         (/ (n k)
            deler)))
    (if (= k 0)
        (/ (n 1) deler)
        (iter n d (- k 1) (next deler))))
  (iter n d k (d k)))

#|   ITERATIEF MET (let)   |#

(define (cont-frac-it* n d k)
  (define (iter n d k deler)
    (let ((next (+ (d (- k 1))
                   (/ (n k)
                      deler))))
      (if (= k 0)
          (/ (n 1) deler)
          (iter n d (- k 1) next))))
  (iter n d k (d k)))