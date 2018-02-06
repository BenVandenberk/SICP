#lang racket
(define (manieren-wisselgeld bedrag) (wt bedrag 7))
                                         
(define (wt bedrag aantal-soorten)
  (cond ((= bedrag 0) 1)
        ((or (< bedrag 0) (= aantal-soorten 0)) 0)
        (else (+ (wt bedrag
                     (- aantal-soorten 1))
                 (wt (- bedrag 
                        (eerstesoort aantal-soorten))
                     aantal-soorten)))))

(define (eerstesoort aantal-soorten)
  (cond ((= aantal-soorten 7) 100)
        ((= aantal-soorten 6) 50)
        ((= aantal-soorten 5) 20)
        ((= aantal-soorten 4) 10)
        ((= aantal-soorten 3) 5)
        ((= aantal-soorten 2) 2)
        ((= aantal-soorten 1) 1)))

  