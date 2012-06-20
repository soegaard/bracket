#lang racket
(require "root-finding.rkt"
         racket/flonum)

;;; EXAMPLE

; Find solution to ln(x)=1 using Brent's algorithm.
(find-root brent-solver            ; algorithm
           (λ (x) (- (log x) 1.0)) ; function
           2.0 3.0                 ; interval
           double-epsilon          ; absolute precision
           double-epsilon          ; relative precision
           100)                    ; max iterations
2.718281828459045 ; value from (planet williams/science:4:5/science)

; Find solution to ln(x)=1 using Newton's algorithm.
(find-root-polish newton-solver    ; algorithm
           (λ (x) (- (log x) 1.0)) ; function
           (λ (x) (/ 1.0 x))       ; derivative
           3.0                     ; initial guess
           (* 2.0 double-epsilon)  ; absolute precision
           (* 2.0 double-epsilon)  ; relative precision
           100)                    ; max iterations
2.718281828459045 

; Find solution to ln(x)=1 using the secant algorithm.
(find-root-polish secant-solver    ; algorithm
           (λ (x) (- (log x) 1.0)) ; function
           (λ (x) (/ 1.0 x))       ; derivative
           3.0                     ; initial guess
           double-epsilon          ; absolute precision
           double-epsilon          ; relative precision
           100)                    ; max iterations
2.718281828459045 



(define (test f x-lower x-upper expected)
  (define types (list bisection-solver brent-solver))
  (define eps-rel (* 10.0 double-epsilon))
  (define eps-abs (* 10.0 double-epsilon))
  (define max-iterations 150)
  (define (find type)
    (find-root type f x-lower x-upper eps-abs eps-rel max-iterations))
  (define (within-tolerance? a b)
    (fl< (flabs (fl- a b)) (fl* eps-rel (flmin (flabs a) (fl+ (flabs b) eps-abs)))))
  (cons expected (map (λ (x) (list x (within-tolerance? x expected)))
                      (map find types))))

(define (test-polish f df x-guess expected)
  (define types (list secant-solver newton-solver))
  (define eps-rel (* 10.0 double-epsilon))
  (define eps-abs (* 10.0 double-epsilon))
  (define max-iterations 150)
  (define (find type)
    (find-root-polish type f df x-guess 0.0 1e-6 max-iterations))
  (define (within-tolerance? a b)
    (fl< (flabs (fl- a b)) (fl* eps-rel (flmin (flabs a) (fl+ (flabs b) eps-abs)))))
  (cons expected (map (λ (x) (list x (within-tolerance? x expected)))
                      (map find types))))


(test (λ (x) (fl- (* x x) 2.0)) 1.0 2.0 (sqrt 2))
(test sin 3.0 4.0 pi)
(test cos 1.0 2.0 (/ pi 2.0))

(test-polish (λ (x) (fl- (* x x) 2.0)) (λ (x) (* 2.0 x)) 1.0  (sqrt 2))
(test-polish sin cos 3.0  pi)
(test-polish cos (λ (x) (fl- 0.0 (sin x))) 1.0  (/ pi 2.0))
