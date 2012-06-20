#lang racket
(require racket/flonum)
(require (only-in (planet williams/science:4:5/science)
                  finite? double-epsilon))

;;; SOLVER
(define-struct solver-type (name iterator make-initial-state))


; Bracketing
(provide/contract 
 (find-root 
  (solver-type? (inexact-real? . -> . inexact-real?) 
                inexact-real? inexact-real? inexact-real? inexact-real? integer? . -> . inexact-real?)))

(provide bisection-solver brent-solver)

; Polishing
(provide/contract 
 (find-root-polish
  (solver-type? (inexact-real? . -> . inexact-real?) (inexact-real? . -> . inexact-real?)
                inexact-real? inexact-real? inexact-real? integer? . -> . inexact-real?)))

(provide secant-solver  newton-solver)
                                

; root testers
(provide/contract 
 (root-test-in-interval? (inexact-real? inexact-real? inexact-real? inexact-real? . -> . boolean?)))

(provide/contract 
 (root-test-residual? (inexact-real? inexact-real? . -> . boolean?)))

(provide/contract 
 (root-test-delta? (inexact-real? inexact-real? inexact-real? inexact-real? . -> . boolean?)))

; reexport
(provide double-epsilon)


; Two classes of root finding algorithms:
;  - root bracketing
;  - root polishing

; Root Bracketing:
;  - guarantee of convergence
;  - begins with bounded region known to contain root
;  - region reduced until root within tolerance is found

; Root Polishing:
;  - an initial guess is iteratively improved to find root
;  - given a good guess and a function compatible with the algorithm: rapid convergence

; Common Framework:
;  - initialize solver state s, for algorithm T
;  - update s using iteration T
;  - test s for convergence, and repeat iteration if necessary

; Search Stopping
;  - a root found within user defined precision
;  - user-specified maximum number of iterations reached
;  - an error has occured

;;;
;;; ROOT TESTING
;;;

; See http://bzr.savannah.gnu.org/lh/gsl/trunk/annotate/head:/roots/convergence.c
; and http://www.gnu.org/software/gsl/manual/html_node/Search-Stopping-Parameters.html


(define (root-test-in-interval? x-lower x-upper eps-abs eps-rel)
  ; Is |a-b| < epsabs + epsrel * min( |a|, |b| ) ?  (If the interval does not contain 0)
  ; Is |a-b| < epsabs ? (If the interval does contain 0)
  ; A root r estimate will then fulfill  |r' - r| < epsabs + epsrel * r'  where r' is the true root
  (let ([abs-lower (flabs x-lower)]
        [abs-upper (flabs x-upper)])
    (when (fl< eps-rel 0.0)
      (error "relative tolerance is negative"))
    (when (fl< eps-abs 0.0)
      (error "absolute tolerance is negative"))
    (when (fl> x-lower x-upper)
      (error "lower bound larger than upper bound"))
    (let* ([min-abs (if (or (and (fl> x-lower 0.0) (fl> x-upper 0.0))
                            (and (fl< x-lower 0.0) (fl< x-upper 0.0)))
                        (flmin abs-lower abs-upper)
                        0.0)]
           [tolerance (+ eps-abs (* eps-rel min-abs))])
      (fl< (flabs (fl- x-upper x-lower)) tolerance))))


(define (root-test-delta? x1 x0 eps-abs eps-rel)
  ; Test for convergence of the sequence ..., x0, x1 with absolute error eps-abs and relative error eps-rel.
  (let ([tolerance (fl+ eps-abs (fl* eps-rel (flabs x1)))])
    (when (fl< eps-rel 0.0)
      (error "relative tolerance is negative"))
    (when (fl< eps-abs 0.0)
      (error "absolute tolerance is negative"))
    (or ; (fl= x1 0.0)
        (fl< (flabs (fl- x1 x0)) tolerance)
        (fl= x0 x1))))


(define (root-test-residual? r eps-abs)
  ; test whether |r| < eps-abs
  ; TODO: write better contract instead
  (when (fl< eps-abs 0.0)
    (error "absolute tolerance is negative"))
  (fl< (flabs r) eps-abs))


;;;
;;; BRACKETING
;;;

(define-struct root-solver        (type f    root x-lower x-upper state) #:mutable)


(define (find-root type f x-lower x-upper eps-abs eps-rel max-iterations)
  (let* ([initial-root  0.0] ; make-initial-state will initilize this to the proper initial value
         [make-initial-state (solver-type-make-initial-state type)]
         [initial-state (make-initial-state f x-lower x-upper (λ (x) (set! initial-root x)))]
         [solver (make-root-solver type f initial-root x-lower x-upper initial-state)])
    (let loop ([iterations 0])
      (if (= iterations max-iterations)
          (root-solver-root solver)
          (let ([x-lower (root-solver-x-lower solver)]
                [x-upper (root-solver-x-upper solver)]
                [root    (root-solver-root solver)])
            (if (root-test-in-interval? x-lower x-upper eps-abs eps-rel)
                root
                (begin
                  (solver-iterate solver)
                  (loop (add1 iterations)))))))))

(define (solver-iterate solver)
  (let ([iterate (solver-type-iterator (root-solver-type solver))])
    (iterate solver
             (root-solver-state solver)
             (root-solver-f solver)
             (root-solver-root solver)
             (root-solver-x-lower solver)
             (root-solver-x-upper solver))))

;;;
;;; POLISHING
;;;

(define-struct root-polish-solver (type f df root state) #:mutable)

(define (find-root-polish type f df root-guess eps-abs eps-rel max-iterations)
  (let* ([initial-root  root-guess] 
         [make-initial-state (solver-type-make-initial-state type)]
         [initial-state (make-initial-state f df root-guess)]
         [solver (make-root-polish-solver type f df initial-root initial-state)])
    (solver-iterate-polish solver)
    (let loop ([iterations 0] [old-root initial-root])
      (if (= iterations max-iterations)
          (root-polish-solver-root solver)
          (let ([root (root-polish-solver-root solver)])
            (if (root-test-delta? root old-root eps-abs eps-rel)
                root
                (begin
                  (solver-iterate-polish solver)
                  (loop (add1 iterations) root))))))))

(define (solver-iterate-polish solver)
  (let ([iterate (solver-type-iterator (root-polish-solver-type solver))])
    (iterate solver
             (root-polish-solver-state solver)
             (root-polish-solver-f solver)
             (root-polish-solver-df solver)
             (root-polish-solver-root solver))))

;;;
;;; ROOT BRACKETING ALGORITHMS
;;;

;;;
;;; BISECTION
;;;

(define-struct bisection-state (f-lower f-upper) #:mutable)

(define (make-initial-bisection-state f x-lower x-upper root!)
  (root! (fl* 0.5 (fl+ x-lower x-upper)))
  (let ([f-lower (f x-lower)]
        [f-upper (f x-upper)])
    (when (or (and (fl< f-lower 0.0) (fl< f-upper 0.0))
              (and (fl> f-lower 0.0) (fl> f-upper 0.0)))
      (error "endpoints do not not straddle y=0"))
    (make-bisection-state f-lower f-upper)))

(define (bisection-iterate solver state f root x-lower x-upper)
  (let ([f-lower (bisection-state-f-lower state)]
        [f-upper (bisection-state-f-upper state)])
    (cond
      [(fl= f-lower 0.0) 
       (set-root-solver-root! x-lower)
       (set-root-solver-x-upper! x-lower)]
      [(fl= f-upper 0.0) 
       (set-root-solver-root!  x-upper)
       (set-root-solver-x-lower! x-upper)]
      [else
       (let* ([x-bisect (fl* 0.5 (fl+ x-lower x-upper))]
              [f-bisect (f x-bisect)])
         (cond
           [(fl= f-bisect 0.0)  
            (set-root-solver-root!  x-bisect)
            (set-root-solver-x-upper! x-bisect)
            (set-root-solver-x-lower! x-bisect)]
           [(or (and (fl> f-lower 0.0) (fl< f-bisect 0.0))
                (and (fl< f-lower 0.0) (fl> f-bisect 0.0)))
            (set-root-solver-root! solver (fl* 0.5 (fl+ x-lower x-bisect)))
            (set-root-solver-x-upper! solver x-bisect)
            (set-bisection-state-f-upper! state f-bisect)]
           [else
            (set-root-solver-root! solver (fl* 0.5 (fl+ x-bisect x-upper)))
            (set-root-solver-x-lower! solver x-bisect)
            (set-bisection-state-f-lower! state f-bisect)]))])))

(define bisection-solver (solver-type "bisection" bisection-iterate make-initial-bisection-state))

;;;
;;; BRENT
;;;

(define-struct brent-state (a b c d e fa fb fc) #:mutable)

(define (make-initial-brent-state f x-lower x-upper root!)
  (root! (fl* 0.5 (fl+ x-lower x-upper)))
  (let ([f-lower (f x-lower)]
        [f-upper (f x-upper)])
    (when (or (and (fl< f-lower 0.0) (fl< f-upper 0.0))
              (and (fl> f-lower 0.0) (fl> f-upper 0.0)))
      (error "endpoints do not not straddle y=0"))
    (make-brent-state x-lower x-upper x-upper (fl- x-upper x-lower) (fl- x-upper x-lower)
                      f-lower f-upper f-upper)))

 
(define (brent-iterate solver state f root x-lower x-upper)
  (let ([tolerance 0.0]
        [m 0.0]
        [ac-equal? #f])
    (let ([a (brent-state-a state)]
          [b (brent-state-b state)]
          [c (brent-state-c state)]
          [d (brent-state-d state)]
          [e (brent-state-e state)]
          [fa (brent-state-fa state)]
          [fb (brent-state-fb state)]
          [fc (brent-state-fc state)])
      (when (or (and (fl< fb 0.0) (fl< fc 0.0))
                (and (fl> fb 0.0) (fl> fc 0.0)))
        (set! ac-equal? #t)
        (set! c a)
        (set! fc fa)
        (set! d (fl- b a))
        (set! e (fl- b a)))
      (when (fl< (flabs fc) (flabs fb))
        (set! ac-equal? #t)
        (set! a b)
        (set! b c)
        (set! c a)
        (set! fa fb)
        (set! fb fc)
        (set! fc fa))
      (set! tolerance (fl* 0.5 (fl* double-epsilon (flabs b))))
      (set! m (fl* 0.5 (fl- c b)))
      (when (fl= fb 0.0)
        (set-root-solver-root! solver b)
        (set-root-solver-x-lower! solver b)
        (set-root-solver-x-upper! solver b))
      (when (fl<= m tolerance)
        (set-root-solver-root! solver b)
        (if (fl< b c)
            (begin
              (set-root-solver-x-lower! solver b)
              (set-root-solver-x-upper! solver c))
            (begin
              (set-root-solver-x-lower! solver c)
              (set-root-solver-x-upper! solver b))))
      (if (or (fl< (flabs e) tolerance)
              (fl< (flabs fa) (flabs fb)))
          (begin
            (set! d m)
            (set! e m))
          (begin
            (let ([s (fl/ fb fa)])
              (let-values ([(p q)
                            (if ac-equal?
                                (values (fl* 2.0 (fl* m s))
                                        (fl- 1.0 s))
                                (let* ([r (fl/ fb fc)]
                                       [q (fl/ fa fc)]
                                       [p (fl* s (fl- (fl* 2.0 (fl* m (fl* q (fl- q r)))) (fl* (fl- b a) (fl- r 1.0))))]
                                       [q (fl* (fl- q 1.0) (fl* (fl- r 1.0) (fl- s 1.0)))])
                                  (values p q)))])
                (if (fl> p 0.0)
                    (set! q (fl- 0.0 q))
                    (set! p (fl- 0.0 p)))
                (if (fl< (fl* 2.0 p)
                         (flmin (fl- (fl* 3.0 (fl* m q)) (flabs (fl* tolerance q)))
                                (flabs (fl* q e))))
                    (begin
                      (set! e d)
                      (set! d (fl/ p q)))
                    (begin
                      (set! d m)
                      (set! e m)))))))
      (set! a b)
      (set! fa fb)
      (if (fl> (flabs d) tolerance)
          (set! b (fl+ b d))
          (set! b (fl+ b (if (fl> m 0.0) tolerance (fl- 0.0 tolerance)))))
      (set! fb (f b))
      (set-brent-state-a! state a)
      (set-brent-state-b! state b)
      (set-brent-state-c! state c)
      (set-brent-state-d! state d)
      (set-brent-state-e! state e)
      (set-brent-state-fa! state fa)
      (set-brent-state-fb! state fb)
      (set-brent-state-fc! state fc)
      (set-root-solver-root! solver b)
      (when (or (and (fl< fb 0.0) (fl< fc 0.0))
                (and (fl> fb 0.0) (fl> fc 0.0)))
        (set! c a))
      (if (fl< b c)
          (begin
            (set-root-solver-x-lower! solver b)
            (set-root-solver-x-upper! solver c))
          (begin
            (set-root-solver-x-lower! solver c)
            (set-root-solver-x-upper! solver b))))))
          
      
(define brent-solver (solver-type "brent" brent-iterate make-initial-brent-state))

;;;
;;; ROOT POLISHING ALGORITHMS
;;;

;;;
;;; SECANT
;;;

(define-struct secant-state (fx dfx) #:mutable)

(define (make-initial-secant-state f df root-guess)
  (make-secant-state (f root-guess) (df root-guess)))
 
(define (secant-iterate solver state f df root)
  (let ([x   root]
        [fx  (secant-state-fx state)]
        [dfx (secant-state-dfx state)])
    ;(display (list x fx dfx)) (newline)
    (when (zero? dfx)
      (error "derivative is zero" 'GSL_EZERODIV))
    (unless (finite? dfx)
      (error "derivative value is not finite"  'GSL_EBADFUNC))
    (let* ([x-new   (fl- x (fl/ fx dfx))]
           [fx-new  (f x-new)]
           [dfx-new (fl/ (fl- fx-new fx)
                         (fl- x-new x))])
      ;(display (list x-new fx-new dfx-new)) (newline)
      (set-root-polish-solver-root! solver x-new)
      (set-secant-state-fx!  state fx-new)
      (set-secant-state-dfx! state dfx-new)
      (unless (finite? fx-new)
        (error "function value is not finite"  'GSL_EBADFUNC)))))

(define secant-solver (solver-type "secant" secant-iterate make-initial-secant-state))

;;;
;;; NEWTON
;;;

(define-struct newton-state (fx dfx) #:mutable)

(define (make-initial-newton-state f df root-guess)
  (make-newton-state (f root-guess) (df root-guess)))
 
(define (newton-iterate solver state f df root)
  (let ([x   root]
        [fx  (newton-state-fx state)]
        [dfx (newton-state-dfx state)])
    (when (zero? dfx)
      (error "derivative is zero" 'GSL_EZERODIV))
    ; root_new = *root - (state->f / state->df);
    (let* ([x-new   (fl- x (fl/ fx dfx))]
           [fx-new  (f x-new)]
           [dfx-new (df x-new)])
      (set-root-polish-solver-root! solver x-new)
      (set-newton-state-fx!  state fx-new)
      (set-newton-state-dfx! state dfx-new)
      (unless (finite? fx-new)
        (error "function value is not finite"  'GSL_EBADFUNC))
      (unless (finite? dfx-new)
        (error "derivative value is not finite"  'GSL_EBADFUNC)))))

(define newton-solver (solver-type "newton" newton-iterate make-initial-newton-state))

