#lang racket

#|  Eigen fabricaat |#

(define (deelbaar? n deler)
  (= (remainder n deler) 0))

(define (GCD a b)
  (cond ((= a b) a)
        ((< a b) (GCDLoop a b 0))
        (else (GCDLoop b a 0))))

(define (GCDLoop a b k)
  (cond ((= k a) 1)
        ((and(deelbaar? a (- a k))(deelbaar? b (- a k)))
         (- a k))
        (else (GCDLoop a b (+ k 1)))))

#|  SICP  |#

(define (gcd a b)
   (if (= b 0)
       a
       (gcd b (remainder a b))))