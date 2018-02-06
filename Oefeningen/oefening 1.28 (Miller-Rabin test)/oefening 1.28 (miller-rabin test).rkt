#lang racket

#|  http://en.wikipedia.org/wiki/Miller-Rabin  |#

(define (square x) (* x x))

(define (nontrivial?-0-else-x x priem)
  (cond ((= x 1) x)                                         #|  (1)  |#
        ((= x (square(- priem 1))) x)                       #|  (2)  |#
        (else (cond ((= (remainder x priem) 1)  0)
                    ((= (remainder x priem) -1) 0)
                    (else x)))))

(define (expmod* base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (nontrivial?-0-else-x (square (expmod* base (/ exp 2) m))
                                          m)
                     m))
        (else (remainder (* base (expmod* base (- exp 1) m))
                          m))))

(define (miller-rabin n)
  (define (try-it a)
    (= (expmod* a (- n 1) n) 0))
  (try-it (+ 2 (random (- n 2)))))

(define (priem? n times)
  (cond ((miller-rabin n)(display "is geen priemgetal"))
        ((= times 0) (display "is een priemgetal"))
        (else (priem? n (- times 1)))))

#|
(1) Triviale square root: 1 mod priem == 1
(2) Triviale square root: vanwege alternatieve versie van Fermat's Little Theorem.
    nl. dat a^(priem-1) mod priem == 1
OPM. De x die binnenkomt in nontrivial?-0-else-x is al een kwadraat vanwege volgorde van oplossen
van recursieve chain opgebouwd in expmod*.
OPM. 2 Bedenk mij na oplossen dat expmod* evengoed moet werken met de nontrivial? na de square, dus
(square(nontrivial?...
Met deze methode zou de nontrivial? procedure er evenvoudiger uitzien.
OPM.3 Na lezen van antwoorden op het internet: i.p.v. een procedure nontrivial zoals ik heb gedaan,
is het ook een mogelijkheid om een aangepaste square procedure te schrijven die, evt. in nog een
andere procedure, test voor een triviale square root en test voor niet triviale square roots en 
een passend resultaat teruggeeft. De expmod procedure ziet er zo overzichtelijker uit.
|#





      


        

      
  
      