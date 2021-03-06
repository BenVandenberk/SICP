#lang racket

(define (square x) (* x x))     
     
(define (expmod a n m)
  (cond ((= n 0) 1)
        ((even? n)
         (remainder (square (expmod a (/ n 2) m))
                    m))
        (else (remainder (* a (expmod a (- n 1) m))
                         m))))

(define (rest? a n)
  (cond ((= a 0) true)
        ((= (expmod a n n) a)
         (rest? (- a 1) n))
        (else false)))

(define (fermat-test-alle-a? x)
  (rest? (- x 1) x))

(define (carmichael? x)
  (and (fermat-test-alle-a? x)
       (not (priemgetal? x))))

(define (zoekcarmichael vanaf)
  (if (carmichael? vanaf) vanaf
      (zoekcarmichael (+ vanaf 1))))

#|/////////////////////////////////////////////////////////////////////////////|#

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

#|/////////////////////////////////////////////////////////////////////////////|#


          
     



