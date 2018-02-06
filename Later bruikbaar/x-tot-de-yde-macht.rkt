#lang racket
(define (totde x y)
  (if (= y 0)
      1
      (* x (totde x (- y 1)))))

#|   Bij wijze van oefening: zelfde functie met lineaire iteratie   |#

(define (totde-it x y)
  (define (totde-it-iter y tussentot)
    (if (> y 0)
        (totde-it-iter (- y 1) (* x tussentot))
         tussentot))
  (totde-it-iter y 1))

#|   Gebruik makend van b^n = [(b^(n/2)]^2 als even
                        b^n = b * b^(n-1)  als oneven   |#

(define (totde-spec x y)
  (define (square x) (* x x))
  (cond ((= y 0) 1)
        ((even? y) (square(totde-spec x (/ y 2))))
        (else (* x (totde-spec x (- y 1))))))

#|  totde-spec maakt gebruik van de methode van 'succesive squaring'.
    Oefening 1.16 vraagt om 'succesive squaring' iteratief uit te voeren   |#

(define (totde-spec-it x y)
  (define (square x) (* x x))
  (define (totde-spec-it-iter x y a)
    (cond ((and(even? y)(> y 0))
           (totde-spec-it-iter (square x) (- (/ y 2) 1) (* a (square x))))
          ((and(not(even? y))(> y 0))
           (totde-spec-it-iter x (- y 1) (* a x)))
          (else a)))
  (totde-spec-it-iter x y 1))

#| Mijn oplossing (boven) werkt; heb er wel echt lang op gezocht. Onder is hetzelfde,
maar veel eenvoudiger en logischer! |#

(define (fast-exp-iter b n)
  (define (square x) (* x x))
  (define (exp b n a)
    (cond ((= n 0) a)
          ((even? n) (exp (square b) (/ n 2) a))
          (else (exp b (- n 1) (* b a)))))
  (exp b n 1))
                     