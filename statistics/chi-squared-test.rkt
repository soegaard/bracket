#lang racket
;;;
;;; This module implements Chi-squared tests for goodness of fit and
;;; independence of categories.
;;;

(require racket/flonum
         (planet williams/science:4:5/science)
         (only-in (planet williams/science/unsafe-ops-utils)
                  real->float)         
         plot)

(provide
 (contract-out
  (chi^2-goodness-of-fit-test 
   (sequence? sequence? natural-number/c . -> . (values real? real?)))))

; (chi^2-goodness-of-fit-test observed expected df)
;  Given:   observed, a sequence of observed frequencies
;           expected, a sequence of expected frequencies
;           df,       the degrees of freedom
;  Result:  X^2      = sum  ((observed - expected)^2 / expected), known as the Pearson cumulative test statistic
;           P-value  = 1-chi^2cdf(X^2,df) , the p-value obtained by comparing the test statistic to a chi-squared distribution
(define (chi^2-goodness-of-fit-test observed expected df)
  (define (flsqr x) (fl* x x))
  (define X^2 (for/sum ([o observed] [e expected])
                (let ([o (real->float o)]
                      [e (real->float e)])
                  (fl/ (flsqr (fl- o e)) 
                       e))))
  (define P-value (fl- 1.0 (chi-squared-cdf X^2 df)))
  (values X^2 P-value))

(define (graphical-chi^2-goodness-of-fit-test observed expected df)
  (let-values ([(X^2 P-value) (chi^2-goodness-of-fit-test observed expected df)])
    (let ([x-max (* 1.50 X^2)])
      (plot (list (function (λ (x) (chi-squared-pdf x df)) 0.0 X^2 #:color 'red )
                  (function-interval (λ (x) (chi-squared-pdf x df)) #:line1-color 'red
                                     (λ (x) 0.0)
                                     X^2 x-max)
                  (points '(#(0.0 0.4))  #:size 0 #:label (format "X^2 = ~a" X^2) )
                  (points '(#(0.0 0.35)) #:size 0 #:label (format "P-value = ~a" P-value) )
                  (lines `(#(,X^2 0.0) #(,X^2 ,(chi-squared-pdf X^2 df))) #:color 'black)
                  )
            #:x-min 0.0
            #:x-max x-max
            #:y-min 0.0
            #:y-max 0.5))))

(define (chi^2-independence-test observed-matrix)
  ; observed-matrix is a sequence of rows.
  ; a row is a sequnce of natural numbers.
  (let* ([nrows (sequence-length observed-matrix)]
         [ncols (sequence-length (sequence-ref observed-matrix 0))]
         [rows  observed-matrix]
         [row-sums (for/vector ([row rows])
                     (for/sum ([o row])
                       o))]
         [col-sums (for/vector ([j ncols])
                     (for/sum ([row rows]) 
                       (sequence-ref row j)))]
         [N (real->float (for/sum ([s (in-vector row-sums)]) s))]
         [expected (for*/vector #:length (* nrows ncols)
                     ([i nrows] [j ncols])
                     (fl/ (fl* (real->float (vector-ref row-sums i))
                               (real->float (vector-ref col-sums j)))
                          N))]
         [expected-matrix (for/vector #:length nrows ([i nrows])
                            (for/vector #:length ncols ([j ncols])
                              (vector-ref expected (+ (* i ncols) j))))]
         [observed (for*/vector #:length (* nrows ncols)
                     ([row rows] [o row])
                     o)]
         [df (* (- nrows 1) (- ncols 1))])
    (let-values ([(X^2 P-value) (chi^2-goodness-of-fit-test observed expected df)])
      (values X^2 P-value df expected-matrix))))



(module* test #f
  ; From   http://en.wikipedia.org/wiki/Pearson%27s_chi-squared_test
  ;   X^2 = 1.22 
  ;   P-value ~ 0.23
  (displayln "Test 1")
  (define observed '(44 56))
  (define expected '(50 50))
  (display (graphical-chi^2-goodness-of-fit-test observed expected 1))
  (newline)
  (chi^2-goodness-of-fit-test observed expected 1)
  
  ; - - - - - - 
  
  ; Expected outcome
  ; X^2     = 10.527145672037388
  ; P-value = 0.005176775866863048
  ; df = 2
  ; Expected =
  ; '#(#(33.536842105263155 20.46315789473684) 
  ;    #(9.936842105263159 6.063157894736842) 
  ;    #(15.526315789473685 9.473684210526315))
  (displayln "\n\nTest 2")
  (chi^2-independence-test #( #(40 14)
                              #(10  6)
                              #(9  16))))


