#lang racket

(define (factorial2 x)
  (if (> x 1) (* x (factorial (- x 1)))
      (* x))) #| x is hier altijd 1, dus 'x' schrijven is nogal zinloos |#

(define (factorial x)
  (if (> x 1) (* x (factorial (- x 1)))
      (* 1)))

(define (fact x)
  (cond ((> x 1) (* x (fact (- x 1))))
        (else (* 1))))

(define (fact2 x)
  (if (= x 1) 1
      (* x (factorial(- x 1)))))

(define (fact3 x) (fact3-iter x 1))

(define (fact3-iter x counter)
  (if (<= counter x) (* counter (fact3-iter x (+ 1 counter)))
      (* 1))) #| Dit is nog altijd met recursie! |#

(define (fact4 x) (fact4-iter 1 1 x))

(define (fact4-iter product counter max-counter)
  (if (> counter max-counter)
      product
      (fact4-iter (* counter product)
                  (+ counter 1)
                  max-counter)))


      

 


