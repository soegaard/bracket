#lang racket

(module common racket/base
  (provide curly?)
  (define (curly? stx)
    (let ([p (syntax-property stx 'paren-shape)])
      (and p 
           (or (eqv? p #\{)
               (and (pair? p)
                    (or (eqv? (car p) #\{)
                        (eqv? (cdr p) #\{))))))))

(module splay racket
  ; Convenient syntax for working with splay-trees  
  ;   {s i}          = (splay-tree-ref s i 0)
  ;   {s i 0}        = (splay-tree-remove! s i) 
  ;   {s i v}        = (splay-tree-set! s i v)
  ;   {count s}      = (splay-tree-count s)
  ;   {remove! s i}  = (splay-tree-remove! s i)
  ;   {for-each s f} = (splay-tree-for-each s f) ; where f : exponent coef -> coef  
  ;                    note: f is called with exponents going from smallest to greatest
  ;   ...
  (provide (rename-out [app #%app]))
  (require (for-syntax (submod ".." common))
           data/splay-tree racket/dict)
  
  (define-syntax (app stx)
    (syntax-case stx 
      (count remove! for-each map 
             greatest-pos least-pos value-at key-at ->list next)
      [{_}
       (curly? stx)
       (syntax/loc stx (make-adjustable-splay-tree))]
      [{_ next s} 
       (curly? stx)
       (syntax/loc stx (splay-tree-iterate-next s))]
      [{_ count s} 
       (curly? stx)
       (syntax/loc stx (splay-tree-count s))]
      [{_ key-at s key} 
       (curly? stx)
       (syntax/loc stx (splay-tree-iterate-key s key))]
      [{_ value-at s pos} 
       (curly? stx)
       (syntax/loc stx (splay-tree-iterate-value s pos))]
      [{_ greatest-pos s} 
       (curly? stx)
       (syntax/loc stx (splay-tree-iterate-greatest s))]
      
      [{_ least-pos s} 
       (curly? stx)
       (syntax/loc stx (splay-tree-iterate-least s))]
      [{_ for-each s f} 
       (curly? stx)
       (syntax/loc stx (dict-for-each s f))]
      [{_ map s f} 
       (curly? stx)
       (syntax/loc stx (dict-map s f))]
      [{_ remove! s v} 
       (curly? stx)
       (syntax/loc stx (splay-tree-remove! s v))]
      [{_ ->list s} 
       (curly? stx)
       (syntax/loc stx (splay-tree->list s))]
      ;; Add new keywords above this line!
      [{_ s i} 
       (curly? stx)
       (syntax/loc stx (splay-tree-ref s i 0))]      
      [{_ s i v} 
       (curly? stx)
       (syntax/loc stx 
         (let ([v1 v])
           (if (zero? v1)
               (splay-tree-remove! s i)
               (splay-tree-set! s i v1))))]
      [(_ . more) 
       (syntax/loc stx (#%app . more))])))


(module poly-base racket
  ; Polynomals in one variable.
  
  ; Polynomials are represented as splay-trees of terms.
  ; Let t be the splay-tree representing the terms of p, then
  ;    ci*x^i is a term of p <=>  {t i} = ci 
  ; All ci stored in t are non-zero, thus 
  ; the zero polynomial is represented by the empty splay-tree.
  
  ; In this module {}-syntax works on splay trees.
  
  ; These function can use the internal representation of polynomials.  
  (provide (struct-out poly) 
           in-terms in-reverse-terms
           current-poly-display default-poly-display infix-poly-display
           make-zero
           degree
           mono
           copy-poly
           k+poly k+poly! poly+k*poly!
           poly+poly poly-poly poly*poly
           k*poly k*poly!
           poly^n slow-poly^n
           deriv
           terms
           leading-term leading-coef leading-exponent
           trailing-term trailing-coef trailing-exponent
           poly-eval  ; horner
           poly/x-r   ; horner
           
           poly-zero?)
  
  (require data/splay-tree racket/dict
           (submod ".." splay))
  
  (define-struct poly (terms) 
    #:property prop:custom-write 
    (λ (s out write?) ((current-poly-display) s out write?))
    #:property prop:sequence (lambda (p)
                               (let ([t (terms p)])
                                 (make-do-sequence
                                  (λ ()
                                    (values
                                     (λ (pos) (mono {key-at t pos} {value-at t pos}))
                                     (λ (pos) (splay-tree-iterate-next t pos))
                                     {least-pos t}
                                     (λ (pos) pos)
                                     #f
                                     #f)))))
    #:methods gen:dict
    [(define (dict-ref p pos [default (λ () (mono 0 0))]) 
       (let ([t (poly-terms p)]) (mono pos {t pos})))]
    #:transparent)
  (define terms poly-terms)
  
  (define (in-terms p)
    (in-dict (terms p)))
  
  (define default-poly-display
    (λ (s out write?)
      (define (display-poly p)
        ((if write? write display) 
         (list 'poly
               (for/list ([(i ci) (in-reverse-terms p)])
                 (list i ci))) out))
      (define write-poly display-poly)
      (if write? (write-poly s) (display-poly s))))
  
  (define infix-poly-display
    (λ (s out write?)
      (define (display-poly p)
        ((if write? write display) 
         (poly->string p "x") out))
      (define write-poly display-poly)
      (if write? (write-poly s) (display-poly s))))
  
  (define (poly->string p [var "x"])
    (define (sign r) (if (>= r 0) "+" "-"))
    (define (exponent->string var n)
      (cond [(= n 0) ""]
            [(= n 1) var]
            [else (string-append var "^" (number->string n))]))
    (let* ([sign-coef-exps
            (for/list ([(i ci) (in-reverse-terms p)])
              (let ([es   (exponent->string var i)]
                    [signs (sign ci)]
                    [cis  (number->string (abs ci))])
                (cond
                  [(= i 0)   (list signs cis "")]
                  [(= ci  1) (list "+" "" es)]
                  [(= ci -1) (list "-" "" es)]
                  [else      (list signs cis es)])))])
      (define (space x)
        (if (equal? x "") "" (string-append " " x)))
      (match sign-coef-exps
        [(list) "0"]
        [(list (list sign coef exps) more ...)
         (string-append 
          (if (equal? sign "+") "" sign) 
          (if (equal? sign "+") coef (space coef))
          exps
          (apply string-append
                 (map (match-lambda
                        [(list sign coef exps)
                         (string-append (space sign) (space coef) exps)])
                      more)))])))
  
  (define current-poly-display 
    (make-parameter default-poly-display))
  
  
  (require (only-in unstable/sequence
                    sequence-lift))
  
  (define (in-reverse-terms p)
    (sequence-lift (λ (c) (values (car c) (cdr c)))
                   (in-list (reverse {->list (terms p)}))))
  
  (define (make-zero) (poly {}))
  
  (define (poly-zero? p)
    (zero? {count (terms p)}))
  
  (define (copy-poly p)
    (let ([q {}])
      (for ([(i ci) (in-dict (terms p))])
        {q i ci})
      (poly q)))
  
  (define (mono i ci)
    (let ([t {}])
      (unless (zero? ci)
        {t i ci})
      (poly t)))
  
  (define (k+poly! k p)
    (let ([t (terms p)])
      {t 0 (+ k {t 0})})
    p)
  
  (define (k+poly k p)
    (k+poly! k (copy-poly p)))
  
  (define (k*poly! k p)
    (let ([t (terms p)])
      (if (zero? k)
          {for-each t (λ (i ci) {remove! t i})}
          {for-each t (λ (i ci) {t i (* k ci)})}))
    p)
  
  (define (k*poly k p)
    (k*poly! k (copy-poly p)))
  
  (define (degree p)
    (let* ([t (terms p)]
           [pos {greatest-pos t}])
      (if pos {key-at t pos} -inf.0)))
  
  (define (coef p i)
    {(terms p) i})
  
  (define (leading-term p)
    (let* ([t (terms p)]
           [pos {greatest-pos t}])
      (if pos 
          (mono {key-at t pos} {value-at t pos})
          (mono 0 0))))  ; or #f ?
  
  (define (leading-coef p)
    (let* ([t (terms p)]
           [pos {greatest-pos t}])
      (if pos 
          {value-at t pos}
          0))) ; ?
  
  (define (leading-exponent p)
    (let* ([t (terms p)]
           [pos {greatest-pos t}])
      (if pos 
          {key-at t pos}
          -inf.0)))
  

  (define (trailing-term p)
    (let* ([t (terms p)]
           [pos {least-pos t}])
      (if pos 
          (mono {key-at t pos} {value-at t pos})
          (mono 0 0))))  ; or #f ?
  
  (define (trailing-coef p)
    (let* ([t (terms p)]
           [pos {least-pos t}])
      (if pos 
          {value-at t pos}
          0))) ; ?
  
  (define (trailing-exponent p)
    (let* ([t (terms p)]
           [pos {least-pos t}])
      (if pos 
          {key-at t pos}
          -inf.0)))

  (define (poly+poly p q)
    (if (> (degree p) (degree q))
        (poly+poly q p)
        ; degree q largest
        (let* ([r (copy-poly q)]
               [t (terms r)]
               [s (terms p)])
          {for-each s (λ (i ci) {t i (+ {t i} {s i})})}
          r)))
  
  (define (poly-poly p q)
    (let* ([r (copy-poly p)]
           [t (terms r)]
           [s (terms q)])
      {for-each s (λ (i ci) {t i (- {t i} {s i})})}
      r))
  
  (define (poly*poly p q)
    (let ([t {}]
          [r (terms p)]
          [s (terms q)])
      (for* ([(i ci) (in-dict r)]
             [(j cj) (in-dict s)])
        (let ([k (+ i j)])
          {t k (+ (* ci cj) {t k})}))
      (poly t)))
  
  (define (slow-poly^n p n)
    ; strictly for testing
    (if (zero? n)
        (mono 0 1)
        (poly*poly p (slow-poly^n p (- n 1)))))
  
  (define (poly^n p n)
    (cond 
      [(= n 0) (mono 0 1)]
      [(= n 1) p]
      [else
       (let ([c {count (terms p)}])
         (cond 
           [(= c 0) p]
           [(= c 1) (slow-poly^n p n)]
           [else
            (let* ([a (leading-term p)]
                   [b (poly-poly (copy-poly p) a)]
                   [a^n (make-hash)]
                   [b^n (make-hash)])
              (hash-set! a^n 0 (mono 0 1)) 
              (hash-set! b^n 0 (mono 0 1))
              (for ([i (in-range 1 (+ n 1))])
                (hash-set! a^n i (poly*poly a (hash-ref a^n (- i 1))))
                (hash-set! b^n i (poly*poly b (hash-ref b^n (- i 1)))))
              (let ([c 1] ; c = (choose n i)
                    [r (mono 0 0)])
                (for ([i (in-range 0 (+ n 1))])
                  (poly+k*poly! r c (poly*poly (hash-ref a^n i)
                                               (hash-ref b^n (- n i))))
                  ; (choose n i+1) / (chose n i) = (n-i)/(i+1)
                  (set! c (/ (* c (- n i))  
                             (+ i 1))))
                r))]))]))
  
  (define (poly+k*poly! p k q)
    (let ([t (terms p)] [s (terms q)])
      {for-each s (λ (i ci) {t i (+ {t i} (* k ci))})}
      p))
  
  (define (deriv p)
    (let ([d {}]
          [t (terms p)])
      {for-each t (λ (i ci) (unless (zero? i) {d (- i 1) (* ci i)}))}
      (poly d)))
  
  (define (poly->list p)
    (for/list ([(i ci) (in-dict (terms p))])
      (cons i ci)))
    
  (define (poly-eval p r)
    ; Horners method
    ; 3*x^3 + 1*x =  x*( 3*x^2 + 1) =  x*( x^2*(3) + 1)
    (if (poly-zero? p)
        0
        (let ([v 0] [k (degree p)])
          (for ([(i ci) (in-reverse-terms p)])
            (set! v (+ ci (* v (expt r (- k i)))))
            (set! k i))
          (* v (expt r (trailing-exponent p))))))
  
  (define (poly/x-r p r)
    ; BUG TODO: test case where p is non-consecutive exponents
    ; Examples from Wikipedia
    ; > (poly/x-r {+ {^ x 3} {* -6 {^ x 2}} {* 11 x} -6} 2)
    ; '(1 -4 3 0)
    ; > (poly/x-r {+ {* 2 {^ x 3}} {* -6 {^ x 2}} {* 2 x} -1} 3)
    ; '(2 0 2 5)
    ; the last number is the remainder
    (let ([j 1] [v 0] [k (degree p)])
      (for/list ([c (in-list (reverse (poly->list p)))])
        (let ([i (car c)] [ci (cdr c)])
          (display (- k i)) (newline)
          (set! v (+ ci (* v (expt r (- k i)))))
          (set! k i)
          v))))
  
  )

(module poly-base-syntax racket
  (require (submod ".." poly-base)
           (for-syntax (submod ".." common)))
  (provide (all-from-out (submod ".." poly-base))
           (rename-out [app #%app])
           for/poly for/poly*)
  
  (define-for-syntax (curlify stx)
    (syntax-property stx 'paren-shape #\{))
  
  
  (define-syntax (for/poly stx)
    (syntax-case stx ()
      [(_ clauses . defs+exprs)
       (with-syntax ([original stx])
         #'(for/fold/derived original ([sum (make-zero)]) clauses
                             (define d (let () . defs+exprs))
                             (values (poly+poly sum d))))]))
  
  (define-syntax (for/poly* stx)
    (syntax-case stx ()
      [(_ clauses . defs+exprs)
       (with-syntax ([original stx])
         #'(for/fold/derived original ([sum (mono 0 1)]) clauses
                             (define d (let () . defs+exprs))
                             (values (poly*poly sum d))))]))
  
  (define-syntax (app stx)
    (syntax-case stx (* + - ^ quot+rem quot rem zero? eval constant)
      [{_ k}
       (and (curly? stx) (number? (syntax-e #'k)))
       (syntax/loc stx (mono 0 k))]
      [{_ e}
       (curly? stx)
       (syntax/loc stx 
         (let ([v e]) (if (number? v) (mono 0 v) (v))))]
      [{_ * k p}
       (and (curly? stx) (number? (syntax-e #'k)))
       (syntax/loc stx (k*poly k p))]
      [{_ * p k}
       (and (curly? stx) (number? (syntax-e #'k)))
       (syntax/loc stx (k*poly k p))]
      [{_ * p q}
       (curly? stx)
       (syntax/loc stx (poly*poly p q))]
      [{_ * p q r ...}
       (curly? stx)
       (syntax/loc stx  {app * {app * p q} r ...})]
      [{_ + k1 k2}
       (and (curly? stx) (number? (syntax-e #'k1)) (number? (syntax-e #'k2)))
       (syntax/loc stx (mono 0 (+ k1 k2)))]
      [{_ + k p}
       (and (curly? stx) (number? (syntax-e #'k)))
       (syntax/loc stx (k+poly k p))]
      [{_ + p k}
       (and (curly? stx) (number? (syntax-e #'k)))
       (syntax/loc stx (k+poly k p))]
      [{_ + p q}
       (curly? stx)
       (syntax/loc stx (poly+poly p q))]
      [{_ + p q r ...}
       (curly? stx)
       (syntax/loc stx  {app + {app + p q} r ...})]
      [{_ - p}
       (curly? stx)
       (syntax/loc stx (k*poly -1 p))]
      [{_ - k1 k2}
       (and (curly? stx) (number? (syntax-e #'k1)) (number? (syntax-e #'k2)))
       (syntax/loc stx (mono 0 (- k1 k2)))]
      [{_ - k p}
       (and (curly? stx) (number? (syntax-e #'k)))
       (syntax/loc stx (k+poly (- k) p))]
      [{_ - p k}
       (and (curly? stx) (number? (syntax-e #'k)))
       (syntax/loc stx (k+poly (- k) p))]
      [{_ - p q}
       (curly? stx)
       (syntax/loc stx (poly-poly p q))]
      [{_ ^ k1 k2}
       (and (curly? stx) (number? (syntax-e #'k1)) (number? (syntax-e #'k2)))
       (syntax/loc stx (mono 0 (expt k1 k2)))]
      [{_ ^ p n}
       (curly? stx)
       (syntax/loc stx (poly^n p n))]
      [{_ zero? p}
       (curly? stx)
       (syntax/loc stx (poly-zero? p))]
      [{_ eval p x0}
       (curly? stx)
       (syntax/loc stx (poly-eval p x0))]
      [{_ constant e}
       (curly? stx)
       (syntax/loc stx (mono 0 e))]
      [(_ . more) 
       (syntax/loc stx (#%app . more))])))

(module poly racket
  (require (submod ".." poly-base-syntax)
           (for-syntax (submod ".." common)))
  (provide (except-out (all-from-out (submod ".." poly-base-syntax)) #%app)
           (rename-out [app #%app])
           poly-quot+rem poly-quot poly-rem poly-gcd
           poly-compose)
  
  #;(define (poly-quot+rem p1 p2)
      (define lc leading-coef)
      (let ([m (degree p1)]
            [n (degree p2)])
        (cond
          [(< m n) (values (make-zero) p1)]
          [(= m n) (let ([q (/ (lc p1) (lc p2))])
                     (values (mono 0 q) (poly-poly p1 (k*poly q p2))))]
          [else    (let* ([q (/ (lc p1) (lc p2))]
                          [p1* (poly-poly p1 (poly*poly (mono (- m n) q) p2))])
                     (let-values ([(q1 r) (poly-quot+rem p1* p2)])
                       (values (poly+poly q1 (mono (- m n) q)) r)))])))
  
  (define (poly-quot+rem p1 p2)
    (define lc leading-coef)
    (let ([m (degree p1)]
          [n (degree p2)])
      (cond
        [(< m n) (values {0} p1)]
        [(= m n) (let ([q (/ (lc p1) (lc p2))])
                   (values {q} {- p1 {* {constant q} p2}}))]
        [else    (let* ([q (/ (lc p1) (lc p2))]
                        [p1* {- p1 {* (mono (- m n) q) p2}}])
                   (let-values ([(q1 r) (poly-quot+rem p1* p2)])
                     (values {+ q1 (mono (- m n) q)} r)))])))
  
  (define (poly-quot p1 p2)
    (let-values ([(q r) (poly-quot+rem p1 p2)])
      q))
  
  (define (poly-rem p1 p2)
    (let-values ([(q r) (poly-quot+rem p1 p2)])
      r))
  
  (define (poly-gcd p1 p2)
    (if (< (degree p1) (degree p2))
        (poly-gcd p2 p1)
        (let loop ([r0 p1] [r1 p2])
          (if {zero? r1}
              r0
              (loop r1 (poly-rem r0 r1))))))
  
  (define (poly-compose p q)
    ;; computes p(q(x))
    (for/poly ([(i ci) (in-terms p)])
      {* {constant ci} {^ q i}}))
  
  (define-syntax (app stx)
    (syntax-case stx (quot+rem quot rem gcd)
      [{_ rem p1 p2}
       (syntax/loc stx (poly-rem p1 p2))]
      [{_ gcd p1 p2}
       (syntax/loc stx (poly-gcd p1 p2))]
      [{_ quot p1 p2}
       (syntax/loc stx (poly-quot p1 p2))]
      [{_ quot+rem p1 p2}
       (syntax/loc stx (poly-quot+rem p1 p2))]      
      [(_ . more) 
       (syntax/loc stx (#%app . more))]))
  
  )

(module summation racket
  (require (submod ".." poly))
  (provide symbolic-sum
           poly-delta poly-shift
           poly-falling-factorial poly-rising-factorial
           binomial stirling-2)
  
  ; binomial : natural natural -> natural
  ;  compute the binomial coeffecient n choose k
  (define (binomial n k)
    ; ;  <http://www.swox.com/gmp/manual/Binomial-Coefficients-Algorithm.html>
    ; TODO: Check range of n and k
    (cond
      [(= k 0)            1]
      [(= k 1)            n]
      [(= k 2)            (/ (* n (- n 1)) 2)]
      [(> k (/ n 2))      (binomial n (- n k))]
      [else               (* (+ n (- k) 1)
                             (for/product ([i (in-range 2 (+ k 1))])                               
                               (/ (+ n (- k) i)
                                  i)))]))
  
  (define (poly-shift p [k 1])
    ;; computes p(x+k)
    (poly-compose p {+ (mono 1 1) {constant k}}))
  
  (define (poly-delta p) 
    ;; computes p(x+1) - p(x)
    {- (poly-shift p) p})
  
  (define (poly-falling-factorial p m)
    ;; computes p(x) * p(x-1) * ... * p(x-m+1) 
    ;; there are m factors in the product
    (for/poly* ([i (in-range m)])
               (poly-shift p (- i))))
  
  (define (poly-rising-factorial p m)
    ;; computes p(x) * p(x+1) * ... * p(x+m-1) 
    ;; there are m factors in the product
    (for/poly* ([i (in-range m)])
               (poly-shift p i)))
  
  (define (stirling-2 m i)
    ; TODO: *very* naïve implementation
    (cond
      [(and (= i 0) (= m 0)) 1]
      [(= i 0)               0]
      [(> i m)               0]
      [else                  (+ (stirling-2 (- m 1) (- i 1))
                                (* i (stirling-2 (- m 1) i)))]))
  
  ;;; (symbolic-sum p) =  Sigma  p(k)
  ;;;                     0<=k<x
  (define (symbolic-sum p)
    (define x (mono 1 1))
    (if {zero? p}
        (make-zero)
        (for/poly ([(i ci) (in-terms p)])
          (for/poly ([j (in-range (+ i 1))])
            {* {constant (* ci (/ (stirling-2 i j) (+ j 1)))}
               (poly-falling-factorial x (+ j 1))}))))
  
  
  ; Sum of the first million cubes.
  ; > (my-poly-eval (symbolic-sum x^3) 1000001)
  ; 250000500000250000000000

  
  )

(module sturm racket
  (require (submod ".." poly))
  (provide sturm-sequence
           count-sign-changes
           count-real-roots-in-interval)
  
  (define (sturm-sequence p)
    (define (next p_n p_n+1)
      {- {rem p_n p_n+1}})
    (define p0 p)
    (define p1 {deriv p})
    (define p*          
      (let loop ([p_n   p0]
                 [p_n+1 p1])
        (if {zero? p_n+1}
            (list p_n)
            (cons p_n (loop p_n+1 (next p_n p_n+1))))))
    p*)
  
  (define (count-sign-changes xs)
    (define (same-sign? r s)
      (or (and (positive? r)
               (positive? s))
          (and (negative? r)
               (negative? s))))
    (match xs
      [(list)    0]
      [(list x1) 0]
      [(list x1 x2 x3 ...) 
       (+ (if (same-sign? x1 x2) 0 1)
          (count-sign-changes (cons x2 x3)))]))
  
  (define (count-real-roots-in-interval p a b)
    ; http://audition.ens.fr/brette/calculscientifique/lecture8.pdf
    ; http://www.di.ens.fr/~ponce/scicomp/notes.pdf
    ; should p be square-free?
    (define x (mono 1 1))
    (define (poly-eval p x0)
      {leading-coef {rem p {- x {x0}}}}) ; TODO: Use fix horner
    (define (count x0)
      (count-sign-changes
       (map (λ (p) (poly-eval p x0))
            (sturm-sequence p))))
    (- (count a) (count b)))
  
  )

(require 'poly)
(require 'sturm)
(require 'summation)

(define x   (mono 1 1))
(define x^2 (mono 2 1))
(define x^3 (mono 3 1))

(current-poly-display infix-poly-display)
(poly-quot+rem {+ {^ x 3} {^ x 2}} {^ x 3})

(define (my-poly-eval p x0)
  (define x (mono 1 1))
  ; TODO
  {leading-coef {rem p {- x {x0}}}})


