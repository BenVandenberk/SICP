#lang racket
(define (posint x)
  (cond ((= 1 x) (display #t))
        ((< x 0) (display #f))
        (else (posint (- x 1)))))