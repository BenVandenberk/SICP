#lang racket
#|/////////////////////////////|#
#|ORGIGINELE GEVONDEN OPLOSSING|#
#|/////////////////////////////|#

(define (iterative-improve good-enough? improve)
  (define (guessloop guess)
    (if (good-enough? guess)
        guess
        (guessloop (improve guess))))
  (lambda (guess) (guessloop guess)))

(define (vierkantswortel x)
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) 0.001))
  (define (improve guess)
    (average guess (/ x guess)))
  ((iterative-improve good-enough? improve) 1.0))

(define (fixed-point f guess)
  (let ((tolerance 0.000001))
    (define (close-enough? guess)
      (< (abs (- guess (next guess))) tolerance))
    (define (next guess)
      (f guess))
    ((iterative-improve close-enough? next) guess)))

#|////Helpertjes////|#

(define (average x y)
  (/ (+ x y) 2))

(define (square x) (* x x))

#|/////////////////////////////////////////////|#
#|OPMERKINGEN BIJ ORGIGINELE GEVONDEN OPLOSSING|#
#|/////////////////////////////////////////////|#

;(1) De laatste regel van iterative-improve is hetzelfde als gewoon zeggen 'guessloop'.
;(2) i.p.v. good-enough?, improve, close-enough? en next te definiëren in 'vierkantswortel' en
;    'fixed-point', kan je evengoed lambda's gebruiken.
; De verbeteringen toegepast zien er zo uit:

(define (iterative-improve* good-enough? improve)
  (define (guessloop guess)
    (if (good-enough? guess)
        guess
        (guessloop (improve guess))))
  guessloop)

(define (vierkantswortel* x)
  ((iterative-improve* (lambda (guess)
                         (< (abs (- (square guess) x)) 0.001))
                       (lambda (guess)
                         (average guess (/ x guess))))
   1.0))

(define (fixed-point* f first-guess)
  ((iterative-improve* (lambda (guess)
                         (< (abs (- guess (f guess))) 0.000001))
                       (lambda (guess) ; Of gewoon f :)
                         (f guess)))
   first-guess))

;(3) Bovendien kan je iterative-improve ook schrijven zonder hulpfuntie (i.c. guessloop)
;    Zo was ik mijn zoektocht begonnen, had het niet gevonden, maar het moet dus als volgt:

(define (iterative-improve** good-enough? improve)
  (lambda (guess)
    (if (good-enough? guess)
        guess
        ((iterative-improve** good-enough? improve) (improve guess)))))

; Het is dus wel mogelijk om de 'lambda' opnieuw aan te roepen!
