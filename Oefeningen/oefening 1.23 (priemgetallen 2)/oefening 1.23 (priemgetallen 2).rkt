#lang racket

#|  Pas de procedure die je in 1.22 geschreven hebt aan zodat (kleinste-deler n) na het gebruiken
    van 2 als test-deler de andere even getallen niet meer gebruikt als test-delers.
                            Levert dit de verwachte efficiëntiewinst op?                         |#

(define (kwadraat x) (* x x))

(define (deelbaar? n deler)
  (= (remainder n deler) 0))

(define (kleinste-deler n)
  (define (volgendedeler x)
    (if (= x 2) 3
        (+ x 2)))
  (define (kleinste-deler-iter n test-deler)
    (cond ((> (kwadraat test-deler) n) n)
          ((deelbaar? n test-deler) test-deler)
          (else (kleinste-deler-iter n (volgendedeler test-deler)))))
  (kleinste-deler-iter n 2))

(define (priemgetal? n)
  (= (kleinste-deler n) n))

(define (zoekpriemgetallen aantal vanaf)
  (if   (even? vanaf)
        (looppriem aantal (+ vanaf 1))
        (looppriem aantal vanaf)))

(define (looppriem aantal vanaf)
  (if (> aantal 0)
      (if 
        (priemgetal? vanaf)
        (begin
          (timed-prime-test vanaf)
          (looppriem (- aantal 1) (+ vanaf 2)))
        (looppriem aantal (+ vanaf 2)))
      (begin
        (newline)
        (display "Klaar"))))

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (current-milliseconds)))
(define (start-prime-test n start-time)
  (if (priemgetal? n)
      (report-prime (- (current-milliseconds) start-time))
      (display " is geen priemgetal")))
(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))