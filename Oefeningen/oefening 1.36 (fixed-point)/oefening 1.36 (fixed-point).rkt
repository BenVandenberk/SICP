#lang racket

(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (begin (display guess)
             (newline)
             (if (close-enough? guess next)
                 next
                 (try next)))))
  (try first-guess))

#|/////////////////////////////////////////|#

(define (fixed-point* f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (begin (display guess)
           (newline)
           (let ((next (f guess)))
                (if (close-enough? guess next)
                    next
                    (try next)))))
  (try first-guess))

#|   Met average damping   |#

(define (fixed-point** f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (begin (display guess)
           (newline)
           (let ((next (average guess (f guess))))
                (if (close-enough? guess next)
                    next
                    (try next)))))
  (try first-guess))

(define (average x y)
  (/ (+ x y) 2))