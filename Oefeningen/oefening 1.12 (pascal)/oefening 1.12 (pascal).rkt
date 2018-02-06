#lang racket
(define (pascal x y)
  (cond ((or (= x y) (= y 1)) 1)
        ((or(> y x)(< x 1)(< y 1))(display "Ongeldige waarde"))
        (else (+ (pascal (- x 1) (- y 1))
                 (pascal (- x 1) y)))))

(define (printrij x y)
  (if (<= y x)
      (begin
        (print(pascal x y))
        (display " ")
        (printrij x (+ y 1)))
      (newline)))

(define (pascaldriehoek startrij eindrij)
  (if (> startrij eindrij)
      (newline)
      (begin
        (spaties (- eindrij startrij))
        (printrij startrij 1)
        (pascaldriehoek (+ startrij 1) eindrij))))

(define (spaties x)
  (if (>= x 1)
      (begin
        (display " ")
        (spaties (- x 1)))
      (display " ")))


  
