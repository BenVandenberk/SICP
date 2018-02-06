#lang racket

(define (cube x) (* x x x))

(define (Simpson-integraal f a b n)
  (define (dbblinc x) (+ x 2))
  (define h (/ (+ b a)
                 n))
  (define (y k)
    (f (+ a (* k h))))
  (define (som-functie functie next k n)
    (if (> k n)
        0
        (+ (functie k)
           (som-functie functie next (next k) n))))
  (define (som-oneven)
    (som-functie y
                 dbblinc
                 1
                 (- n 1)))
  (define (som-even)
    (som-functie y
                 dbblinc
                 2
                 (- n 2)))
  (* (+ (f a)
        (f b)
        (* 4 (som-oneven))
        (* 2 (som-even)))
     (/ h 3)))

#|///////////////////////////////////////////////////////

In mijn oplossing ontbind ik de som in 4 delen:
- k=1 --> f(a)
- k=n --> f(b)
- 1>k<n, k even --> coëfficiënt 2
- 1>k<n, k oneven --> coëfficiënt 4

Een andere manier is om de functie 'functie' ('term' in sum-procedure in het boek) als volgt te schrijven:
(define (term k)
  (* (cond ((odd? k) 4)
           ((even? k) 2)
           ((or(= k 1)(= k n)) 1))
     (y k)))

/////////////////////////////////////////////////////////|#







  

         