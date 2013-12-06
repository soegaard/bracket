#lang racket
(require rackunit)
; Adaptive Simpson's Method
; See Felleisen's post:
; https://groups.google.com/forum/?fromgroups#!topic/plt-scheme/JXwe9nZmCnQ

(provide/contract
 [adaptive-simpson 
  (->i ((f (-> real? real?)) (L real?) (R  (L) (and/c real? (>/c L))))
       (#:epsilon (ε real?))
       (r real?))])

(define (adaptive-simpson f L R #:epsilon [ε .000000001])
  (define f@L (f L))
  (define f@R (f R))
  (define-values (M f@M whole) (simpson-1call-to-f f L f@L R f@R))
  (asr f L f@L R f@R ε whole M f@M))

;; computationally efficient: 2 function calls per step 
(define (asr f L f@L R f@R ε whole M f@M)
  (define-values (leftM  f@leftM  left*)  (simpson-1call-to-f f L f@L M f@M))
  (define-values (rightM f@rightM right*) (simpson-1call-to-f f M f@M R f@R))
  (define delta* (- (+ left* right*) whole))
  (cond
    [(<= (abs delta*) (* 10 ε)) (+ left* right* (/ delta* 15.0))]

    [else (define epsilon1 (/ ε 2))
          (+ (asr f L f@L M f@M epsilon1 left*  leftM  f@leftM) 
             (asr f M f@M R f@R epsilon1 right* rightM f@rightM))]))

(define (simpson-1call-to-f f L f@L R f@R)
  (define M (mid L R))
  (define f@M (f M))
  (values M f@M (* (/ (abs (- R L)) 6.0) (+ f@L (* 4.0 f@M) f@R))))

(define (mid L R) (/ (+ L R) 2.))

;; simplistic prototype: many calls to f per step 
;; (asr f L R ε whole)

;; Simpson's rule for approximating an integral
#;(define (simpson f L R)
    (* (/ (- R L) 6.0) (+ (f L) (* 4.0 (f (mid L R))) (f R))))

#;(define (asr.v0 f L R ε whole)
    (define M  (mid L R))
    (define left*  (simpson f L M))
    (define right* (simpson f M R))
    (define delta* (- (+ left* right*) whole))
    (cond
      [(<= (abs delta*) (* 15 ε)) (+ left* right* (/ delta* 15))]      
      [else (define epsilon1 (/ ε 2))
            (+ (asr f L M epsilon1 left*) (asr f M R epsilon1 right*))]))

(module* test #f
  (define const=5 (lambda (x) 5))
  (check-equal? (adaptive-simpson const=5 0 1) 5.0)
  (check-equal? (adaptive-simpson const=5 0 10) 50.0)
  
  (check-equal? (adaptive-simpson values 0 1) .5)
  
  (define step (lambda (x) (if (< x 1) (sin x) 1)))
  (adaptive-simpson step 0 2)
  
  (adaptive-simpson (λ (x) (* (/ 100. (* x x)) (sin (/ 10. x)))) 1.0 3.0)
  )