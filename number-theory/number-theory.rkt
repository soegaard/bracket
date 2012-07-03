#lang racket/base
(provide binomial)
  
; binomial : natural natural -> natural
;  compute the binomial coeffecient n choose k
(define (binomial n k)
  ; <http://www.swox.com/gmp/manual/Binomial-Coefficients-Algorithm.html>
  ; http://lavica.fesb.hr/cgi-bin/info2html?(gmp)Binomial%20Coefficients%20Algorithm
  ; TODO: Check range of n and k
  (cond
    [(= k 0)       1]
    [(= k 1)       n]
    [(= k 2)       (/ (* n (- n 1)) 2)]
    [(> k (/ n 2)) (binomial n (- n k))]
    [else          (* (+ n (- k) 1)
                      (for/product ([i (in-range 2 (+ k 1))])
                        (/ (+ n (- k) i)
                           i)))]))

(module* test #f
  (require rackunit)
  ; Binomial
  (check-equal? (binomial 5 0) 1)
  (check-equal? (binomial 5 1) 5)
  (check-equal? (binomial 5 2) 10)
  (check-equal? (binomial 5 3) 10)
  (check-equal? (binomial 5 4) 5)
  (check-equal? (binomial 5 5) 1)
  (check-equal? (binomial 10 6) 210))
