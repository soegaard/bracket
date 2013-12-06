#lang racket

;;;
;;; DEFINITE INTEGRAL
;;;

;;; References
; [NA] Numerical Analysis 7th ed. Burden and Faires

; Composite Simpson's Rule
; Purpose: Approximate the integral I of f(x) from a to b.
; Input:   a,b endpoints
;          n   even positive integer
; Output:  Approximation to I
; Notes:   If f is relatively smooth over [a,b] (e.g. when [a,b] small)
;          then a good approximation is found.
; URL:     http://en.wikipedia.org/wiki/Simpson's_rule
; 
; THEOREM
;   Let f is C^4 on [a,b], n even, h=(b-a)/n and x_j=a+jh for i=0,1,...n.
;   Then there exists μ in ]a,b[ for which the Composite Simpson's rule
;   for n subintervals can be written with its error term as:
;
;      b                             n/2-1              n/2
;   int  f(x) dx  =  h/3 [ f(a)  +  2 sum f(x_2j)  +  4 sum f(x_{2j-1}) +  f(b) ] - error
;      a                              j=1               j=1
;
;                    b-a       (4)
;   where  error =  ----- h^4 f   (μ).
;                    180

(define (simpson f a b [n 100])
  (let* ([a (exact->inexact a)]
         [b (exact->inexact b)]
         [h (exact->inexact (/ (- b a) n))]
         [sum-even 0.0]
         [sum-odd  0.0])
    (for ([i (in-range 1 n)])
      (let* ([x (+ a (* i h))]
             [fx (f x)])
        (if (even? i)
            (+= sum-even fx)
            (+= sum-odd fx))))
    (/ (* h (+ (f a) (f b) (* 2 sum-even) (* 4 sum-odd))) 3.0)))

(define (adaptive f a b tol n)
  (let* ([a (* 1.0 a)]
         [b (* 1.0 b)]
         [tol (* 1.0 tol)]
         [app 0.0])
    (let* ([i 1] 
           [toli (* 10.0 tol)]
           [ai a]
           [hi (/ (- b a) 2.0)]
           [fai (f a)]
           [fci (f (+ a hi))]
           [fbi (f b)]
           [si  (/ (* hi (+ fai (* 4.0 fci fbi))) 3.0)]
           [li 1.0])
      (let loop ([i i])
        (when (> i 0)
          (:= fd (f (+ ai (/ hi 2.0))))
          (:= fe (f (+ ai (/ (* 3.0 hi) 2.0))))
          (:= s1 (/ (* hi (+ fai (* 4.0 fd) fci)) 6.0))
          (:= s2 (/ (* hi (+ fci (* 4.0 fe) fbi)) 6.0))
          (define-values (v1 v2 v3 v4 v5 v6 v7 v8)
            (values ai fai fci fbi hi toli si li))
          (+= i -1)
          (if (< (abs (- (+ s1 s2) v7)) v6)
              (+= app (+ s1 s2))
              (cond
                [(>= v8 n)
                 'error-level-exceeded]
                [else
                 (+= i 1)
                 (:= ai (+ v1 v5))
                 (:= fai v3)
                 (:= fci fe)
                 (:= fbi v4)
                 (:= hi (/ v5 2.0))
                 (:= toli (/ v6 2.0))
                 (:= si s2)
                 (:= li (+ v8 1.0))
                 
                 



;;; Convenient summation

(define-syntax (+= stx)
  (syntax-case stx ()
    [(_ id expr)
     (identifier? #'id)
     (syntax/loc stx (set! id (+ id expr)))]
    [_ (raise-syntax-error #f "Expected (+= id expr)" stx)]))   

(module* test #f
  (require rackunit)
  
  (define (error f F a b [n 100])
    (- (simpson f a b n) (- (F b) (F a))))
  
  (define-values (a b) (values 0 1))
  (check-equal? (error (λ (x) 1) (λ (x) x) a b) 0.0)
  (check-pred (λ (err) (<= err 0.00002)) (error (λ (x) (sin x)) (λ (x) (- (cos x))) 0 pi 20)))
  