#lang racket
(define (even x)
  (posint (/ x 2)))
        

(define (posint x)
  (cond ((= 1 x) (= 1 1))
        ((< x 0) (= 1 2))
        (else (posint (- x 1)))))
                  
