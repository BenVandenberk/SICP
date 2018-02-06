#lang racket

(define (filtered-accumulate predicate combiner null-value term a next b)
  (if (> a b)
      null-value
      (if (predicate a)
          (combiner (term a)
                    (filtered-accumulate predicate combiner null-value term (next a) next b))
          (filtered-accumulate predicate combiner null-value term (next a) next b))))

(define (sum-even term a next b)
  (filtered-accumulate even? + 0 term a next b))

(define (square x) (* x x))

(define (inc x) (+ x 1))

(define (sum-sq-prime a b)
  (filtered-accumulate priemgetal? + 0 square a inc b))

#|//////////////////////////////////////////////////////////////////|#

(define (kwadraat x) (* x x))

(define (deelbaar? n deler)
  (= (remainder n deler) 0))

(define (kleinste-deler n)
  (define (kleinste-deler-iter n test-deler)
    (cond ((> (kwadraat test-deler) n) n)
          ((deelbaar? n test-deler) test-deler)
          (else (kleinste-deler-iter n (+ test-deler 1)))))
  (kleinste-deler-iter n 2))

(define (priemgetal? n)
  (= (kleinste-deler n) n))

#|//////////////////////////////////////////////////////////////////|#

(define (GCD a b)
  (cond ((= a b) a)
        ((< a b) (GCDLoop a b 0))
        (else (GCDLoop b a 0))))

(define (GCDLoop a b k)
  (cond ((= k a) 1)
        ((and(deelbaar? a (- a k))(deelbaar? b (- a k)))
         (- a k))
        (else (GCDLoop a b (+ k 1)))))

(define (identity x) x)

(define (prod-pos-relaprime n)
  (define (RelaPrime? a)
    (= (GCD a n) 1))
  (filtered-accumulate RelaPrime? * 1 identity 1 inc (- n 1)))
  
  

