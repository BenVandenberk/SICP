#lang racket
#|  Gebruik makende van 

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))
(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime) start-time))))
(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))   

schrijf een procedure die in een bepaalde range (bv +1000 of +10000) alle oneven nummers test
voor 'primality' gebruik makende van de methode van het zoeken naar de kleinste deler met
'theta'(sqrt n)   |#

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




