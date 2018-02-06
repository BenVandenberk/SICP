#lang racket

(define (make-rat teller noemer)
  (cons teller noemer))

(define (teller x)
  (car x))

(define (noemer x)
  (cdr x))

(define (add-rat x y)
  (make-rat (+ (* (teller x) (noemer y))
               (* (teller y) (noemer x)))
            (* (noemer x) (noemer y))))

(define (sub-rat x y)
  (make-rat (- (* (teller x) (noemer y))
               (* (teller y) (noemer x)))
            (* (noemer x) (noemer y))))

(define (mul-rat x y)
  (make-rat (* (teller x) (teller y))
            (* (noemer x) (noemer y))))

(define (div-rat x y)
  (make-rat (* (teller x) (noemer y))
            (* (teller y) (noemer x))))

(define (eq-rat? x y)
  (= (* (teller x) (noemer y))
     (* (teller y) (noemer x))))
              

(define (print-rat x)
  (display (teller x))
  (display "/")
  (display (noemer x)))

(define ééntweede (make-rat 1 2))
(define éénderde (make-rat 1 3))