#lang racket

(define (deriv f)
  (let ((dx 0.00001))
    (lambda (x) (/ (- (f (+ x dx))
                      (f x))
                   dx))))

(define (square x) (* x x))

(define (cube x) (* x x x))

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

(define (newton-transform f)
  (lambda (x) (- x
                (/ (f x)
                   ((deriv f) x)))))

(define (newton-method f guess)
  (fixed-point (newton-transform f) guess))

#| (newton-method) zoekt het nulpunt van een functie
   Het is nu mogelijk om sqrt te definiëren ifv (newton-method)
   vertrekkend vanuit volgend idee

   de sqrt x betekent zoek een y zodat y^2=x ofwel y^2 - x = 0  
   m.a.w. het nulpunt in y van de functie y^2 - x = 0    |#

(define (vierkantswortel x)
  (newton-method (lambda (y) (- (square y)
                                x)) 1.0))

(define (cubic a b c)
  (lambda (x) (+ (cube x)
                 (* a (square x))
                 (* b x)
                 c)))