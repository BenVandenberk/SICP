#lang racket

(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
         (product term (next a) next b))))

#|/////////////////////////////////////////////////////////|#

(define (identity x) x)

(define (inc x) (+ x 1))

(define (factorial x)
  (product* identity 1 inc x))

#|/////////////////////////////////////////////////////////|#

(define (inc2 x) (+ x 2))

(define (square x) (* x x))

(define (pi-approx n)
  (define (teller)
    (* 4 (+ n 1)
       (product* square 4 inc2 (* 2 n))))
  (define (noemer)
    (product* square 3 inc2 (+ 1 (* 2 n))))
  (* 4.0
     (/ (teller) (noemer))))

#|/////////////////////////////////////////////////////////|#

(define (product* term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* (term a) result))))
  (iter a 1))

#|/////////////////////////////////////////////////////////|#

(define (pi-approx-beter n)
  (define (pi-term k)
    (/ (* (- k 1)(+ k 1))
       (square k)))
  (* 4.0
     (product* pi-term 3 inc2 (+ 1 (* n 2)))))