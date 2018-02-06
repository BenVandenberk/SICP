#lang racket
#|  Pas resultaat van 1.22 aan door naar priemgetallen te zoeken m.b.v. 
    Fermat test i.p.v. zoeken naar kleinste deler  |#
#|  Fermat test: 
                kies een willekeurige a < n
                is rest (a^n)/n gelijk aan a?
                nee? -> n is zeker geen priemgetal
                ja? -> herhaal x keer; hoe hoger x, hoe zekerder dat n een priemgetal is  |#

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
        (snel-priemgetal? vanaf 10)
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
  (if (snel-priemgetal? n 10)
      (report-prime (- (current-milliseconds) start-time))
      (display " is geen priemgetal")))
(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

#|(define (snel-priemgetal? n)
 (define (snelpriemtest n keren)
   (cond ((= keren 0) #t)
         ((rest-modulo? (+ 1 (random (- n 1))) n)
          (snelpriemtest n (- keren 1)))
         (else #f)))
 (snelpriemtest n 10))        
           
(define (fast-exp-iter b n)
  (define (square x) (* x x))
  (define (exp b n a)
    (cond ((= n 0) a)
          ((even? n) (exp (square b) (/ n 2) a))
          (else (exp b (- n 1) (* b a)))))
  (exp b n 1))
  
(define (rest-modulo? a n)
  (= (remainder (fast-exp-iter a n) n)
     a))|#
(define (square x) (* x x))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (snel-priemgetal? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (snel-priemgetal? n (- times 1)))
        (else false)))





