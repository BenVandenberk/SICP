#lang racket
(define (totde x y)
  (if (= y 0)
      (* 1)
      (* x (totde x (- y 1)))))

(define (arrow x y)
  (if (= y 1)
      (totde x x)
      (totde (arrow x (- y 1)) x)))