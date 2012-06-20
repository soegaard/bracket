#lang racket
(require (planet williams/science/unsafe-ops-utils)
         racket/flonum)

#;(define (flexpt a x) ; huh?
  (real->float (expt a x)))

(define (flexpt a x)
  (flexp (fl* x (fllog a))))

(define-syntax-rule (fl~ x)
  (fl- 0.0 x))

(define-syntax (swap! stx)
  (syntax-case stx ()
    [(_ x y) #'(let ([t x]) (set! t x) (set! x y) (set! y t))]))

(provide/contract 
 (solve-cubic (real? real? real? . -> . (listof real?))))

; (solve-cubic a b c) returns list of the real solutions of x^3 + a*x^2 + b*x + c = 0
(define (solve-cubic a b c)
  (let ([a (real->float a)]
        [b (real->float b)]
        [c (real->float c)])
    (let* ([q (fl- (fl* a a) (fl* 3.0 b))]
           [r (fl+ (fl- (fl* 2.0 (fl* a (fl* a a)))
                        (fl* 9.0 (fl* a b)))
                   (fl* 27.0 c))]
           [Q (fl/ q 9.0)]
           [R (fl/ r 54.0)]
           [Q3 (fl* (fl* Q Q) Q)]
           [R2 (fl* R R)]
           [CR2 (fl* (fl* 729.0 r) r)]
           [CQ3 (fl* 2916.0 (fl* q (fl* q q)))])
      
      (cond 
        [(and (fl= R 0.0) (and (fl= Q 0.0)))
         (let ([r (fl/ (fl~ a) 3.0)])
           (list r r r))]
        [(fl= CR2 CQ3)
         ; this test is actually R2 == Q3, written in a form suitable
         ; for exact computation with integers

         ; Due to finite precision some double roots may be missed, and
         ; considered to be a pair of complex roots z = x +/- epsilon i
         ; close to the real axis. 
         (let ([sqrtQ (flsqrt Q)])
           (if (fl> R 0.0)
               (list (fl- (fl* -2.0 sqrtQ) (fl/ a 3.0))
                     (fl- sqrtQ (fl/ a 3.0))
                     (fl- sqrtQ (fl/ a 3.0)))
               (list (fl- (fl~ sqrtQ) (fl/ a 3.0))
                     (fl- (fl~ sqrtQ) (fl/ a 3.0))
                     (fl- (fl* 2.0 sqrtQ) (fl/ a 3.0)))))]
        [(fl< R2 Q3)
         (let* ([sgnR (if (fl>= R 0.0) 1.0 -1.0)]
                [ratio (fl* sgnR (flsqrt (fl/ R2 Q3)))]
                [theta (acos ratio)]
                [norm (fl* -2.0 (flsqrt Q))]
                [r0 (fl- (fl* norm (cos (fl/ theta 3.0))) (fl/ a 3.0))]
                [r1 (fl- (fl* norm (cos (fl/ (fl+ theta (fl* 2.0 pi)) 3.0))) (fl/ a 3.0))]
                [r2 (fl- (fl* norm (cos (fl/ (fl- theta (fl* 2.0 pi)) 3.0))) (fl/ a 3.0))])
           (when (fl> r0 r1)
             (swap! r0 r1))
           (when (fl> r1 r2)
             (swap! r1 r2)
             (when (fl> r0 r1)
               (swap! r0 r1)))
           (list r0 r1 r2))]
        [else
         (let* ([sgnR (if (fl>= R 0.0) 1.0 -1.0)]
                [A (fl* (fl~ sgnR) (flexpt (fl+ (flabs R) (flsqrt (fl- R2 Q3))) (fl/ 1.0 3.0)))]
                [B (fl/ Q A)])
           (list (fl- (fl+ A B) (fl/ a 3.0))))]))))


;;; Test cases from GSL all pass
; (equal? (solve-cubic 0.0 0.0 -27.0)        '(3.0))
; (equal? (solve-cubic -51.0 867.0 -4913.0)  '(17.0 17.0 17.0))
; (equal? (solve-cubic -57.0 1071.0 -6647.0) '(17.0 17.0 23.0))
; (equal? (solve-cubic -11.0 -493.0 +6647.0) '(-23.0 17.0 17.0))
; (solve-cubic -143.0 5087.0 -50065.0)  ; => '(16.999999999999993 31.000000000000004 95.0)
; '(17.0 31.0 95.0)
; (solve-cubic -109.0 803.0 50065.0) ; => '(-16.999999999999993 30.999999999999996 95.0)
; '(-17.0 31.0 95.0)
