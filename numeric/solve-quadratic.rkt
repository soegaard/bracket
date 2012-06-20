#lang racket

(require (planet williams/science/unsafe-ops-utils)
         racket/flonum)

(provide/contract 
 (solve-quadratic (real? real? real? . -> . (listof real?))))

(define-syntax-rule (fl~ x)
  (fl- 0.0 x))

; (solve-quadratic a b c) returns list of the real solutions of a*x^2 + b*x + c = 0
; not all of a, b, and, c must be zero.
(define (solve-quadratic a b c)
  (let ([a (real->float a)]
        [b (real->float b)]
        [c (real->float c)])
    (if (fl= a 0.0) 
        (if (fl= b 0.0)
            '()
            (list (fl/ (fl~ c) b)))
        (let ([d (fl- (fl* b b) (fl* (fl* 4.0 a) c))])
          (cond
            [(fl> d 0.0) (if (fl= b 0.0)
                             (let ([r (flsqrt (fl/ (fl~ c) a))])
                               (list (fl~ r) r))
                             (let* ([sign-b (if (fl> b 0.0) 1.0 -1.0)]
                                    [tmp (fl* -0.5 (fl+ b (fl* sign-b (flsqrt d))))]
                                    [r1 (fl/ tmp a)]
                                    [r2 (fl/ c tmp)])
                               (if (fl< r1 r2)
                                   (list r1 r2)
                                   (list r2 r1))))]
            [(fl= d 0.0) (list (fl/ (fl* -0.5 b) a))]
            [else        '()])))))


;;;
;;; TEST
;;;

(require (planet williams/science/random-source)
         (planet williams/science/math))

(define (test)
  (define eps 1e-9)
  (define (random-real-between from to)
    (real->float (+ from (* (- to from) (random-real)))))
  (define (rand) (random-real-between -10 10))
  (let ([a (rand)] [r1 (rand)] [r2 (rand)])
    (let ([r1 (min r1 r2)] [r2 (max r1 r2)])
      (let ([b (fl* a (fl~ (fl+ r1 r2)))]
            [c (fl* a (fl* r1 r2))])
        (andmap (λ (x1 x2)
                  (unless (zero? (fcmp x1 x2 eps))
                    (display (list '(a r1 r2) '= (list a r1 r2))) (newline)
                    (display (list '(a b c) '  = (list a b c))) (newline)
                    (display (list '(solve-quadratic a b c) '= (solve-quadratic a b c))) (newline)
                    (display (list `(fcmp ,x1 ,x2 ,eps) '= (fcmp x1 x2 eps))) (newline))
                  (zero? (fcmp x1 x2 eps)))
                (solve-quadratic a b c)
                (list r1 r2))))))

; a r1 r2 ((0.8002497503654915 -0.5820734661704101 4.291708989226137) 
; a b  c   (0.8002497503654915 -2.968634901272305 -1.999095840595189))
;(test)

; (solve-quadratic 0.8002497503654915 -2.968634901272305 -1.999095840595189)

; (fcmp -0.5820734661704101 -0.5820734661704101 1e-15)


;;; cases to investigate

;((a r1 r2) = (3.8566316203631885 -8.106416563069132 -8.032453290827172))
;((a b c) = (3.8566316203631885 62.24167579546271 251.12230179873728))
;((solve-quadratic a b c) = (-8.106416563068963 -8.03245329082734))
;((fcmp -8.106416563068963 -8.106416563069132 1e-14) = 1)
;
;((a r1 r2) = (-1.6265548994586378 -9.696396453504093 -9.60199798858156))
;((a b c) = (-1.6265548994586378 -31.38989803145976 -151.44003484077456))
;((solve-quadratic a b c) = (-9.696396453503917 -9.601997988581735))
;((fcmp -9.696396453503917 -9.696396453504093 1e-14) = 1)
;
;((a r1 r2) = (8.297284009362357 5.2152202289471905 5.247945289959347))
;((a b c) = (8.297284009362357 -86.81585594733481 227.08994615882847))
;((solve-quadratic a b c) = (5.215220228947072 5.247945289959466))
;((fcmp 5.215220228947072 5.2152202289471905 1e-14) = -1)
;
;((a r1 r2) = (8.441076002955395 -4.497852045007335 -4.494381895016747))
;((a b c) = (8.441076002955395 75.90413012409832 170.6368983607456))
;((solve-quadratic a b c) = (-4.497852045006565 -4.494381895017516))
;((fcmp -4.497852045006565 -4.497852045007335 1e-14) = 1)
