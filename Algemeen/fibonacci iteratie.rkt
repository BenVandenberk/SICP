#lang racket
    
#|(define (fib n) (fib-iter 0 1 n))

(define (posint x)
  (cond ((= 1 x) (= 1 1))
        ((< x 0) (= 1 2))
        (else (posint (- x 1)))))

(define (fib-iter a b counter)
  (cond ((not(posint counter)) (display "The Argument of 'fib' must be a positive integer"))
        ((= counter 0) a)
        ((= counter 1) b)
        ((> counter 2) (fib-iter b (+ a b) (- counter 1)))
        (else (+ a b))))|#

#|   VERBETERING NA BLOCK STRUCTURE EN LEXICAL SCOPING   |#

(define (posint x)
  (cond ((= 0 x) (= 1 1))
        ((< x 0) (= 1 2))
        (else (posint (- x 1)))))

(define (fib-beter n)
  (define (fib-iter a b counter)
      (if 
        (> counter 0) (fib-iter b (+ a b) (- counter 1))
         a))
  (if (not(posint n)) (display "The Argument of 'fib' must be a positive integer")
      (fib-iter 0 1 n)))   

#|BOEK OPLOSSING|#

(define (fib2 n)
  (fib-iter2 1 0 n))

(define (fib-iter2 a b count)
  (if (= count 0)
      b
      (fib-iter2 (+ a b) a (- count 1))))


