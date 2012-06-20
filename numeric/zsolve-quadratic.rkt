#lang racket

(require (planet williams/science/unsafe-ops-utils)
         racket/flonum)

(provide/contract 
 (zsolve-quadratic (real? real? real? . -> . (listof complex?))))

(define-syntax-rule (fl~ x)
  (fl- 0.0 x))

; (zsolve-quadratic a b c) returns list of the complex solutions of a*x^2 + b*x + c = 0
; where a, b, and, c are real numbers

(define (zsolve-quadratic a b c)
  (let ([a (real->float a)]
        [b (real->float b)]
        [c (real->float c)])
    (let ([d (fl- (fl* b b) (fl* (fl* 4.0 a) c))])
      (if (fl= a 0.0) 
          (if (fl= b 0.0)
              '()
              (list (fl/ (fl~ c) b)))
          (cond
            [(fl> d 0.0) (if (fl= b 0.0)
                             (let ([s (flabs (fl/ (fl* 0.5 (flsqrt d)) a))])
                               (list (fl~ s) s))
                             (let* ([sgnb (if (fl> b 0.0) 1.0 -1.0)]
                                    [temp (fl* -0.5 (fl+ b (fl* sgnb (flsqrt d))))]
                                    [r1 (fl/ temp a)]
                                    [r2 (fl/ c temp)])
                               (if (fl< r1 r2)
                                   (list r1 r2)
                                   (list r2 r1))))]
            [(fl= d 0.0)  (let ([r (fl/ (fl* -0.5 b) a)]) (list r))]   ; double root
            [else         (let ([s (flabs (fl/ (fl* 0.5 (flsqrt (fl~ d))) a))]
                                [r (fl/ (fl* -0.5 b) a)])
                            (list (+ r (* s 0-1i))
                                  (+ r (* s 0+1i))))])))))

; All tests from GSL passes
#;(and (equal? (zsolve-quadratic 4.0 -20.0 26.0) '(2.5-0.5i 2.5+0.5i))
       (equal? (zsolve-quadratic 4.0 -20.0 25.0) '(2.5))
       (equal? (zsolve-quadratic 4.0 -20.0 21.0) '(1.5 3.5))
       (equal? (zsolve-quadratic 4.0 7.0 0.0) '(-1.75 -0.0))
       (equal? (zsolve-quadratic 5.0 0.0 -20.0) '(-2.0 2.0))
       (equal? (zsolve-quadratic 5.0 0.0 20.0) '(-0.0-2.0i -0.0+2.0i))
       (equal? (zsolve-quadratic 0.0 3.0 -21.0) '(7.0))
       (equal? (zsolve-quadratic 0.0 0.0 1.0) '()))
