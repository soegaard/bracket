#lang racket

;;; An ATOMIC EXPRESSION is an
;  - number (integer, real, complex)
;  - reserved symbol (pi, e, i, inf, true, false)
;  - identifier

;;; A COMPOUND EXPRESSION
;  - (operand expression ...)
;    where operand can be
;      - operators: + - * / ^ !
;      - function forms
;      - relational operators and expressions: = ≠ < ≤ > ≥
;      - logical operations and expressions: or and not
;      - 'Set   
;      - 'List

; Variable Initialization and assignment
;  - all variables are initially undefined symbols
;  - assignments :=

; Representation:
;   Identifiers are represented as Racket symbols
;   Numbers     are represented as Racket numbers
;   Operators   are represented as Racket symbols
;   Lists       are represented as ?
;   ...

; Simplified expressions
; (Plus  op1 op2 op ...) 
;   - n-ary, n>=2
;   - at most one operand is a number
;   - no operands are sums
; (Times op1 op2 op ...) 
;   - n-ary, n>=2
;   - no operands are products
;   - at most one operand is a number
;   - when a number is an operand, it is the first operand
; (Power a n)
;   - if n is an integer,
;     then a is not an integer, rational, product or power.
; Minus
;   - unary and binary does not occur in simplified expressions
;   - (Minus x) => (Times -1 x)
;   - (Minus a b) => (Plus a (Times -1 b))
; Quotient
;   - binary quotient does not appear is simplified expressions
;   - (Quotient a b) => (Times a (Power b -1))

; COMPLETE SUBEXPRESSION 
;   Let u be an automatically simplified expression.
;   A complete sub-expression of u is either the
;   expression u itself or an operand of some
;   operator in u.

(module number-theory racket/base
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
                             i)))])))

(module undefined racket/base
  (provide undefined undefined?)
  (define undefined 'undefined)
  (define (undefined? e) (eq? e 'undefined)))

(module identifiers racket
  (provide symbolic-id? reserved?)  
  (define (reserved? v)
    (and (symbol? v)
         (memv v '(@pi @e @i @inf))))
  (define (symbolic-id? id)
    (or (symbol? id)
        (and (syntax? id)
             (memv (syntax-e id)
                   '(Plus Minus Times Quotient Power =))))))

(module symbolic-application racket
  (provide (rename-out [sym-app #%app]))
  (require (for-syntax (submod ".." identifiers)))
  
  (define (holdable? o)
    (and (symbol? o)
         (memq o '(Hold))))
  
  ; In the BRACKET language an application of
  ; a non-function evaluates to an expression.
  (define-syntax (sym-app stx)
    (syntax-case stx ()
      [(_ op arg ...)
       (quasisyntax/loc stx
         (let ([o op])
           (if (procedure? o)
               #,(syntax/loc stx (#%app o arg ...))
               (if (holdable? o)
                   (cons o '(arg ...))
                   (cons o (list arg ...))))))]))
  
  ; This version prevents duplication of args.
  ; But uses apply in place of #%app
  #;(define-syntax (sym-app stx)
    (syntax-case stx ()
      [(_ op arg ...)
       (quasisyntax/loc stx
         (let ([o op]
               [as (λ () (list arg ...))])
           ; TODO: 1. If o is Flat, then flattened nested expression with o.
           ; TODO: 2. If o is Listable, then ...
           ; TODO: 3. If o is Orderless then ...
           ; TODO: 4. If o has associated rules ...
           ; TODO: 
           ; REF: http://reference.wolfram.com/mathematica/tutorial/Evaluation.html
           (if (procedure? o)
               #,(syntax/loc stx (apply o (as)))
               (if (holdable? o)
                   (cons o '(arg ...))
                   (cons o (as))))))])))               

(module expression-core racket
  (require (submod ".." identifiers)
           (submod ".." undefined))
  (provide atomic-expression?
           compound-expression?
           list-expression?
           set-expression?
           power-expression?
           times-expression?
           plus-expression?
           base exponent
           term const
           construct
           kind
           operator
           operands
           number-of-operands
           operand last-operand
           free-of
           complete-sub-expressions)
  (define (atomic-expression? v)
    (or (number? v)
        (reserved? v)
        (symbolic-id? v)
        (boolean? v)))
  
  (define (set-expression? v)
    (and (list? v) (eq? (first v) 'Set)))
  (define (list-expression? v)
    (and (list? v) (not (empty? v)) (eq? (first v) 'List)))
  (define (power-expression? v)
    (and (list? v) (eq? (first v) 'Power)))
  (define (times-expression? v)
    (and (list? v) (eq? (first v) 'Times)))
  (define (plus-expression? v)
    (and (list? v) (eq? (first v) 'Plus)))
  
  (define (base u)
    (cond
      [(number? u) undefined]
      [(power-expression? u) (operand u 0)]
      [else u]))
  
  (define (exponent u)
    (cond
      [(number? u) undefined]
      [(power-expression? u) (operand u 1)]
      [else 1]))
  
  (define (term u)
    (cond
      [(number? u) undefined]
      [(and (times-expression? u)
            (number? (operand u 0)))
       (let ([v (rest (operands u))])
         (if (empty? (rest v))
             (first v)
             (construct 'Times v)))]
      [else u]))
  
  (define (const u)
    (cond
      [(number? u) undefined]
      [(times-expression? u)
       (define u1 (operand u 0))
       (if (number? u1) u1 1)]
      [else 1]))
  
  (define recent-compound-expressions (make-weak-hasheq))
  (define (compound-expression? e)
    (or (hash-has-key? recent-compound-expressions e)
        (let ([compound? (list? e)]) ; TODO: Improve check
          (when compound?
            (hash-set! recent-compound-expressions e #t))
          compound?)))
        
  (define (construct operator operands)
    (cons operator operands))
  
  (define (operator e)
    (and (list? e) (first e)))
  (define (operands e)
    ; Old defi: (and (list? e) (rest e))
    (if (list? e) (rest e) '()))    
  (define (number-of-operands e)
    (if (list? e)
        (length (rest e))
        undefined))
  (define (operand u i)
    (if (compound-expression? u)
        (list-ref (rest u) i)
        undefined))
  (define (last-operand u)
    (operand u (- (number-of-operands u) 1)))
  (define (kind u)
    (cond
      [(number? u)
       (cond [(integer? u)    'integer]
             [(and (exact? u) (rational? u)) 'rational]
             [(rational? u)   'real]
             [(complex? u)    'complex]
             [else (error 'kind "Internal error: A number type is missing: " u)])]
      [(reserved? u)        'reserved]
      [(symbolic-id? u)     'symbolic-id]
      [(list? u)            (operator u)]
      [else (error 'kind "Internal error: Unhandled simplified expression type: " u)]))
  
  (define (complete-sub-expressions u)
    ; return a list of all complete sub-expressions of u
    (if (atomic-expression? u)
        (list u)
        (cons u
              (append-map complete-sub-expressions
                          (operands u)))))
  
  (define (free-of u t)
    ; is t identical to a complete subexpression of u,
    ; if so return #f otherwise #t
    (cond
      [(equal? u t) #f]
      [(atomic-expression? u) #t]
      [else (andmap (curryr free-of t) 
                    (operands u))])))

(module simplify racket
  (require (submod ".." expression-core)
           (submod ".." undefined)
           (submod ".." identifiers))
  (require (planet dherman/memoize:3:1))

  (provide simplify 
           simplify-plus
           simplify-minus
           simplify-times
           simplify-quotient
           simplify-power
           before?)
  
  (define (simplify e)
    (cond
      [(atomic-expression? e) 
       e]
      [(compound-expression? e)
       (let ([op (simplify (operator e))]
             [us (map simplify (operands e))])
         (case op
           [(Plus)     (simplify-plus us)]
           [(Times)    (simplify-times us)]
           [(Power)    (simplify-power us)]
           [(Minus)    (simplify-minus us)]        
           [(Quotient) (simplify-quotient us)]
           [else       (construct op us)]))]
      [else
       (error 'simplify "received non-expression, in: " e)]))
  
  (define (original-if-equal original new)
    (if (equal? original new)
        original 
        new))
  
  (define (flatten-operator operator ops)
    ; Example (f (f x1 x2) x3 (f x4)) -> (f x1 x2 x3 x4)
    (define (loop ops flattened)
      (cond
        [(empty? ops)
         (original-if-equal (reverse flattened) ops)]
        [(eq? (kind (first ops)) operator)
         (loop (rest ops)
               (append (operands (first ops)) flattened))]
        [else
         (loop (rest ops) (cons (first ops) flattened))]))
    (loop ops '()))
  
  (define (simplify-plus ops) 
    ; (Plus op1 op2 op ...) 
    ;   - n-ary, n>=2
    ;   - at most one operand is a number
    ;   - no operands are sums
    ;   - when an operand is a number, it is the first operand
    (cond
      [(ormap undefined? ops) undefined]
      [else
       (define n (length ops))
       (case n
         [(0) 0]
         [(1) (first ops)]
         [else 
          (define vs (simplify-plus-rec ops))
          (if (list-expression? vs)
              vs
              (case (length vs)
                [(1)  (first vs)]
                [(0)  0]            
                [else (construct 'Plus vs)]))])]))

  (define (simplify-plus-rec us)
    ; a list of terms is received,
    ; a list of simplified terms is returned.
    (define n (length us)) ; n>=2
    (define u1 (first us))
    (define u2 (second us))
    (cond
      [(and (= n 2) 
            (not (plus-expression? u1))
            (not (plus-expression? u2)))
       (cond
         [(and (number? u1) (number? u2))
          (let ([s (+ u1 u2)])
            (if (equal? s 0) '() (list s)))]
         [(equal? u1 0) (list u2)]
         [(equal? u2 0) (list u1)]
         [(equal? (term u1) (term u2))
          (define c (simplify-plus  (list (const u1) (const u2))))
          (define t (simplify-times (list c (term u1))))
          (if (equal? t 0) '() (list t))]
         [(and (list-expression? u1) (list-expression? u2)
               (= (length u1) (length u2)))
          (construct
           'List (append-map (λ (u1i u2i) (simplify-plus-rec (list u1i u2i)))
                             (operands u1) (operands u2)))]
         [(and (list-expression? u1) (list-expression? u2))
          ; lists of different lengths => do nothing
          (list u1 u2)]
         [(list-expression? u1)
          (construct 
           'List
           (map (λ (u1i) 
                  (let ([us (simplify-plus-rec (cons u2 (list u1i)))])
                    ; If length(us)>=2 wrap with Plus
                    (if (empty? (rest us))
                        (first us)
                        (construct 'Plus us))))
                (operands u1)))]
         [(list-expression? u2)
          (construct
           'List
           (map 
            (λ (u2i) 
              (let ([us (simplify-plus-rec (cons u1 (list u2i)))])
                ; If length(us)>=2 wrap with Plus
                    (if (empty? (rest us))
                        (first us)
                        (construct 'Plus us))))
            (operands u2)))]
         [(before? u2 u1) (list u2 u1)]
         [else            (list u1 u2)])]
      [(= n 2) 
       ; at least one of u1 or u2 is a sum
       (cond 
         [(and (plus-expression? u1) (plus-expression? u2))
          (merge-sums (operands u1) (operands u2))]
         [(plus-expression? u1)
          (merge-sums (operands u1) (list u2))]
         [(plus-expression? u2)
          (merge-sums (list u1) (operands u2))]
         [else (error)])]
      [else
       (define w (simplify-plus-rec (rest us)))
       (if (plus-expression? u1)
           (merge-sums (operands u1) w)
           (merge-sums (list u1) w))]))
  
  (define (merge-sums p q)
    ; receives two lists of terms
    (cond 
      [(empty? p) q]
      [(empty? q) p]
      [else
       (define p1 (first p))
       (define q1 (first q))
       (define h (simplify-plus-rec (list p1 q1)))
       (case (length h)
         [(0) (merge-sums (rest p) (rest q))]
         [(1) (cons (first h) (merge-sums (rest p) (rest q)))]
         [(2) (if (equal? (first h) q1)
                  (cons q1 (merge-sums p (rest q)))
                  (cons p1 (merge-sums (rest p) q)))]
         [else (error)])]))

  (define/memo (simplify-times ops) 
    ;(displayln (list 'simplify-times ops))
    ; the operands os are simplified
    ; (Times op1 op2 op ...) 
    ;   - n-ary, n>=2
    ;   - no operands are products
    ;   - at most one operand is a number
    ;   - when a number is an operand, it is the first operand
    ;(define os (flatten-operator 'Times ops))
    ;(define-values (ns es) (partition number? os))
    ;(define n (apply * ns))
    (cond
      [(ormap undefined? ops) undefined]
      [(ormap (λ (u) (and (number? u) (zero? u))) ops) 0]
      [else
       (define n (length ops))
       (case n
         [(0) 1]
         [(1) (first ops)]
         [else 
          (define vs (simplify-times-rec ops))
          (if (list-expression? vs)
              vs
              (case (length vs)
                [(1)  (first vs)]
                [(0)  1]
                [else (construct 'Times vs)]))])]))
  
  (define (simplify-times-rec us)
    ; a list of factors are received,
    ; a list of simplified factors are returned.
    (define n (length us)) ; n>=2
    (define u1 (first us))
    (define u2 (second us))
    (cond
      [(and (= n 2) 
            (not (times-expression? u1))
            (not (times-expression? u2)))
       (cond
         [(and (number? u1) (number? u2))
          (let ([p (* u1 u2)])
            (if (equal? p 1) '() (list p)))]
         [(equal? u1 1) (list u2)]
         [(equal? u2 1) (list u1)]
         [(and (list-expression? u1) (list-expression? u2)
               (= (length u1) (length u2)))
          (construct
           'List (append-map (λ (u1i u2i) (simplify-times-rec (list u1i u2i)))
                             (operands u1) (operands u2)))]
         [(and (list-expression? u1) (list-expression? u2))
          ; lists of different lengths => do nothing
          (list u1 u2)]
         [(list-expression? u1)
          (construct 
           'List
           (map (λ (u1i) 
                  (construct 'Times
                             (simplify-times-rec (cons u2 (list u1i)))))
                (operands u1)))]
         [(list-expression? u2)
          (construct
           'List
           (map (λ (u2i) 
                  (construct 'Times
                             (simplify-times-rec (cons u1 (list u2i)))))
                (operands u2)))]
         [(equal? (base u1) (base u2))
          (define s (simplify-plus  (list (exponent u1) (exponent u2))))
          (define p (simplify-power (list (base u1) s)))
          (if (equal? p 1) '() (list p))]
         [(before? u2 u1) (list u2 u1)]
         [else            (list u1 u2)])]
      [(= n 2) 
       ; at least one of u1 or u2 is a product
       (cond 
         [(and (times-expression? u1) (times-expression? u2))
          (merge-products (operands u1) (operands u2))]
         [(times-expression? u1)
          (merge-products (operands u1) (list u2))]
         [(times-expression? u2)
          (merge-products (list u1) (operands u2))]
         [else (error)])]
      [else
       (define w (simplify-times-rec (rest us)))
       (if (times-expression? u1)
           (merge-products (operands u1) w)
           (merge-products (list u1) w))]))
  
  (define (merge-products p q)
    ; receives two lists of factors
    (cond 
      [(empty? p) q]
      [(empty? q) p]
      [else
       (define p1 (first p))
       (define q1 (first q))
       (define h (simplify-times-rec (list p1 q1)))
       (if (list-expression? h)
           (simplify-times-rec (cons h (append (merge-products (rest p) (rest q)))))
           (case (length h)
             [(0) (merge-products (rest p) (rest q))]
             [(1) (let ([m (merge-products (rest p) (rest q))])
                    (if (list-expression? m)
                        (simplify-times-rec (cons (first h) m))
                        (cons (first h) m)))]
             [(2) (if (equal? (first h) q1)
                      (let ([m (merge-products p (rest q))])
                        (if (list-expression? m)
                            (simplify-times-rec (cons q1 m))
                            (cons q1 m)))
                      (let ([m (merge-products (rest p) q)])
                        (if (list-expression? m)
                            (simplify-times-rec (cons q1 m))
                            (cons p1 m))))]
             [else (error 'merge-products (format "got ~a and ~a" p q))]))]))
  
  (define (before? u v)
    ;(display (list 'before? u v))
    ; This an order relation between expressions.
    ; See [Cohen] for the complete algorithm and explanation.
    (define result
      (cond
      [(and (number? u) (number? v))
       ; straightforward for two real numbers,
       ; but complex numbers must be handled too.
       (if (= u v)
           #t
           (if (real? u)
               (if (real? v) (< u v) v)
               (if (real? v)
                   v
                   (if (= (imag-part u) (imag-part v))
                       (< (real-part u) (real-part v))
                       (< (imag-part u) (imag-part v))))))]
      ; Number always come first
      [(number? u) #t]
      [(number? v) #f]
      ; Symbols are sorted in alphabetical order
      [(and (symbol? u) (symbol? v))
       (string<? (symbol->string u) (symbol->string v))]
      ; For products and sums order on the last different
      ; factor or term. Thus x+y < y+z.
      [(or (and (times-expression? u) (times-expression? v))
           (and (plus-expression? u)  (plus-expression? v)))
       (define first-non-equal
         (for/first ([ui (in-list (reverse (operands u)))]
                     [vi (in-list (reverse (operands v)))]
                     #:unless (equal? ui vi))
           (list ui vi)))
       (if first-non-equal
           (apply before? first-non-equal)
           (< (length (operands u)) (length (operands v))))]
      ; When comparing a product with something else, use the last factor.
      ; Thus     x*y < z   and   y < x*z.
      ; Note: This is consistent with comparisons of two products since
      ;          x*y < 1*z and 1*y < x*z.
      [(and (times-expression? u)
            (or ;(power-expression? v)
                ;(plus-expression? v)
                (symbolic-id? v)
                (compound-expression? v)))
       (let ([un (last-operand u)])
         (or (equal? un v) (before? un v)))]
      [(and (times-expression? v)
            (or ;(power-expression? v)
                ;(plus-expression? v)
                (symbolic-id? u)
                (compound-expression? u)))
       (define vn (last-operand v))
       (or (equal? vn u) (before? u vn))]
      ; Powers with smallest base are first.  2^z < 3^y
      [(and (power-expression? u) (power-expression? v))
       (if (equal? (base u) (base v))
           (before? (exponent u) (exponent v))
           (before? (base u) (base v)))]
      ; When comparing a product with something else, pretend
      ; the else part is a power with exponent 1.
      ; Thus x^2 > x.
      [(and (power-expression? u)
            (or ; (plus-expression? v)
                (symbolic-id? v)
                (compound-expression? v)))
       (before? u (construct 'Power (list v 1)))]
      [(and (power-expression? v)
            (or ; (plus-expression? v)
                (symbolic-id? u)
                (compound-expression? u)))
       (before? (construct 'Power (list u 1)) v)]
      ; Same trick with sums. Thus x+(-1) < x (+0) 
      [(and (plus-expression? u)
            (or (symbolic-id? v)
                (compound-expression? v)))
       (before? u (construct 'Plus (list v)))]
      [(and (plus-expression? v)
            (or (symbolic-id? u)
                (compound-expression? u)))
       (before? (construct 'Plus (list u)) v)]
      ; Here only function applications are left.
      ; Sort after name.
      [(and (compound-expression? u) (compound-expression? v))
       (if (not (equal? (kind u) (kind v)))
           (before? (kind u) (kind v))
           (let ()
             ; If the names are equal, sort after the first
             ; non-equal operand.
             (define first-non-equal
               (for/first ([ui (in-list (operands u))]
                           [vi (in-list (operands v))]
                           #:unless (equal? ui vi))
                 (list ui vi)))
             (if first-non-equal
                 (apply before? first-non-equal)
                 (< (length (operands u)) (length (operands v))))))]
      [(compound-expression? u)
       #f]
      [(compound-expression? v)
       #t]
      [else
       (error 'before? "Internal error: A case is missing, got ~a and ~a" u v)]
      ; TODO : This isn't done
      ; some rules are missing ... functions????
      ))
    ;(displayln (format " => ~a" result))
    result
    )
  
  
  
  (define (simplify-minus ops) 
    ; - unary and binary does not occur in simplified expressions
    ; - (Minus x) => (Times -1 x)
    ; - (Minus a b) => (Plus a (Times -1 b))
    (case (length ops)
      [(1) (simplify-times (list -1 (first ops)))]
      [(2) (simplify-plus 
            (list (first ops) 
                  (simplify-times (list -1 (second ops)))))]
      [else undefined]))
  
  (define (simplify-power ops)
    (case (length ops)
      [(2)
       (let ([v (first ops)]
             [w (second ops)])
         (cond
           [(undefined? v)  undefined]
           [(undefined? w)  undefined]
           [(and (equal? v 0) (number? w) (positive? w)) 0]
           [(equal? v 0)    undefined]
           [(equal? v 1)    1]
           [(list-expression? v)
            (construct 
             'List (map (λ (vi) (simplify-power (list vi w)))
                        (operands v)))]
           [(integer? w)    (simplify-integer-power v w)]
           
           [else (construct 'Power (list v w))]))]
      [else undefined]))
  
  (define (simplify-integer-power v n)
    ; n integer, v≠0
    (cond
      [(number? v) (expt v n)]
      [(= n 0) 1]
      [(= n 1) v]
      [(power-expression? v)
       (let* (; w = r^s
              [r (operand v 0)]
              [s (operand v 1)]
              [p (simplify-times (list s n))])
         (if (integer? p)
             (simplify-integer-power r p)
             (construct 'Power (list r p))))]
      [(times-expression? v)
       (simplify-times
        (map (λ (f) (simplify-power (list f n)))
             (operands v)))]      
      [else (construct 'Power (list v n))]))
  
  (define (simplify-quotient ops) 
    ; - binary quotient does not appear is simplified expressions
    ; - (Quotient a b) => (Times a (Power b -1))
    (case (length ops)
      [(1) (simplify-power (list (first ops) -1))]
      [(2) (simplify-times 
            (list (first ops) 
                  (simplify-power (list (second ops) -1))))]
      [else undefined])))

(module expression racket
  (require (submod ".." expression-core)
           (submod ".." simplify))
  (provide (all-from-out (submod ".." expression-core))
           (all-from-out (submod ".." simplify))
           substitute
           sequential-substitute
           concurrent-substitute)
  
  (define (substitute u t r)
    ; return new expression where each occurrence
    ; of the target expression t in u is replaced
    ; with the replacement r. The substituion takes
    ; place whenever t is structurally identical to
    ; a complete sub-expression of u.
    (cond
      [(equal? u t) r]
      [(atomic-expression? u) u]
      [else 
       (simplify
        (construct
         (kind u) 
         (map (λ (ui) (substitute ui t r))
              (operands u))))]))
  
  (define (sequential-substitute u ts rs)
    (if (empty? ts)
        u
        (sequential-substitute 
         (substitute u (first ts) (first rs))
         (rest ts) (rest rs))))
  
  (define (concurrent-substitute u ts rs)
    (cond
      [(empty? ts) u]
      [(for/first ([t ts]
                   [r rs]
                   #:when (equal? u t))
         r) => values]
      [(atomic-expression? u) u]
      [else
       (simplify
        (construct
         (kind u) 
         (map (λ (ui) (concurrent-substitute ui ts rs))
              (operands u))))])))
  


#;(module pattern-matching racket
  (require (submod ".." expression))
  (define (linear-form u x)
    ; u expression, x a symbol
    (if (eq? u x) 
        (list 1 0)
        (case (kind u)
          [(symbol-id integer fraction real complex)
           (list 0 u)]
          [(Times)
           (if (free-of u x)
               (list 0 u)
               (let ([u/x (Quotient u x)])
                 (if (Free-of u/x x)
                     (list u/x 0)
                     #f)))]
          [(Plus)
           (let ([f (linear-form (operand u 1) x)])   
             (and f
                  (let ([r (linear-form (Minus u (operand u 1)))])
                    (and r
                         (list (+ (operand f 0) (operand r 0))
                               (+ (operand f 1) (operand r 1)))))))]
          [else
           (and (free-of u x)
                (list 0 u))]))))

(module equation-expression racket
  (require (submod ".." expression))
  (provide equations->sides
           equation->sides)
  
  (define (equation->sides t=r)
    (values (operand t=r 0) (operand t=r 1)))  
  
  (define (equations->sides t=r-List)
    (values (map (curryr operand 0) (operands t=r-List)) 
            (map (curryr operand 1) (operands t=r-List)))))

(module mpl-graphics racket
  (require "../graphics.rkt")
  
  (define-syntax (declare/provide-vars stx)
    (syntax-case stx ()
      [(_ id ...)
       #'(begin
           (define id 'id) ...
           (provide id) ...)]))
  
  (provide Graphics)
  (declare/provide-vars
   Blend Darker Hue Lighter
   Circle Disk Line Point Rectangle
   Text Thickness
   ; Colors
   Red Blue Green Black White Yellow
   ; Options
   ImageSize PlotRange  
  ))
  
(module bracket racket
  (require (submod ".." number-theory)
           (submod ".." expression)
           (submod ".." undefined)
           (submod ".." equation-expression)
           (submod ".." mpl-graphics))
  (provide ; (all-from-out (submod ".." symbolic-application))
           (rename-out [free-of Free-of]
                       [base Base]
                       [const Const]
                       [term Term]
                       [exponent Exponent]
                       [before? Before?]
                       [kind Kind])
           (all-from-out (submod ".." mpl-graphics))
           Operand
           Operands
           Hold
           Complete-sub-expressions
           Substitute
           Sequential-substitute
           Concurrent-substitute
           Cons
           List 
           List-ref
           Plus Minus Times Quotient Power
           Equal
           Expand
           Set Member?
           Variables
           Map
           Apply
           Append
           AppendStar
           Sin Cos Tan Sqrt
           Solve-quadratic
           Solve-linear
           List->Set
           Define
           Range
           Plot)
  
  ;;;
  ;;; INVARIANT 
  ;;;
  ;;; The following invariant holds for functions in the bracket module.
  ;;;
  ;;; All functions receiving expressions can rely
  ;;; on the expressions to be in automated simplified form (ASF).
  ;;;
  ;;; All functions creating expressions, must make sure
  ;;; the expressions returned are in ASF.
  ;;;
  
  (define Operand operand)
  (define Kind    kind)
  
  (define (Operands u)
    (construct 'List (operands u)))
  
  (define Hold 'Hold)
  
  (define (Plus . expressions)
    (simplify-plus expressions))
  
  (define (Minus . us)
    (simplify-minus us))
  
  (define (Times . us)
    (simplify-times us))
  
  (define (Quotient u1 u2)
    (simplify-quotient (list u1 u2)))
  
  (define (Power u1 u2)
    (simplify-power (list u1 u2)))
  
  (define (List . us)
    (construct 'List us))
  
  (define (Cons u1 u2)
    (construct 'List (cons u1 (List->list u2))))
    
  (define (Set . us)
    (construct 'Set (set->list (list->set us))))
  
  (define (Complete-sub-expressions u)
    (construct 'List (complete-sub-expressions u)))
  
  (define (Substitute u t=r)
    (define-values (t r) (equation->sides t=r))
    (substitute u t r))
  
  (define (Sequential-substitute u t=r-list)
    (define-values (ts rs) (equations->sides t=r-list))
    (sequential-substitute u ts rs))
  
  (define (Concurrent-substitute u t=r-list)
    (define-values (ts rs) (equations->sides t=r-list))
    (concurrent-substitute u ts rs))
  
  (define (Equal u1 u2)
    (cond
      [(and (number? u1) (number? u2))
       (= u1 u2)]
      [(and (string? u1) (string? u2))
       (string=? u1 u2)]
      [else (construct 'Equal (list u1 u2))]))
  
  (define (Expand u)
    ; [Cohen, Elem, p.253]
    (case (kind u)
      [(Plus) 
       (define v (Operand u 0))
       (Plus (Expand v)
             (Expand (Minus u v)))]
      [(Times)
       (define v (Operand u 0))
       (Fix-point ; TODO: neeeded ?
        (λ (u) (cond
                 [(times-expression? u)
                  (Expand-product (Operand u 0) 
                                  (Quotient u (Operand u 0)))]
                 [(power-expression? u) (Expand-power u)]
                 [else u]))
        (Expand-product (Expand v)
                        (Expand (Quotient u v))))]
      [(Power)
       (define base (Operand u 0))
       (define exponent (Operand u 1))
       (if (and (eq? (Kind exponent) 'integer)
                (>= exponent 2))
           (Expand-power (Expand base) exponent)
           u)]
      [else u]))
  
  (define (Expand-product r s)
    ; [Cohen, Elem, p.253]
    (cond
      [(eq? (Kind r) 'Plus)
       (define f (Operand r 0))
       (Plus (Expand-product f s)
             (Expand-product (Minus r f) s))]
      [(eq? (Kind s) 'Plus)
       (Expand-product s r)]
      [else
       (Fix-point  ; TODO: neeeded ?
        (λ (u)
          (if (power-expression? u)
              (let ([n (exponent u)])
                (if (and (integer? n) (>= n 0))
                    (Expand-power (base u) n)
                    u)
                u)
              u))
        (Times r s))]))
  
  (define (Fix-point f u)
    ; Apply f repeatedly to u until
    ; a value u1 with f(u1)=u1 is found.
    ; The the fixpoint u1 is returned.
    (define u1 (f u))
    (if (equal? u u1)
        u
        (Fix-point f u1)))
  
  (define (Expand-power u n)
    ; [Cohen, Elem, p.253]
    (unless (and (integer? n) (>= n 0))
      (error 'Expand-power
             "expected natural number as exponent, got ~a" n))
    (cond
      [(eq? (Kind u) 'Plus)
       ; u^n = (f +(u-f))^n = sum C(n,k) f^i (u-f)^(n-i)
       (define f (Operand u 0))
       (define r (Minus u f))
       (define s 0)
       (for ([k (+ n 1)])
         (define c (binomial n k))
         (set! s 
               (Plus s 
                     (Fix-point  ; TODO: neeeded ?
                      (λ (u)
                        (if (times-expression? u)
                            (Expand u)
                            u))
                      (Expand-product 
                       (Times c (Power f (- n k)))
                       (Expand-power r k))))))
       s]
      [else
       (Power u n)]))
  
  (define (Member? x s)
    (and (member x (operands s)) #t))
  
  (define (Rational? u)
    (and (number? u)
         (exact? u)))
  
  (define (Append L1 L2)
    (construct 'List
               (append (Operands L1)
                       (Operands L2))))
  
  (define (AppendStar Ls)
    (list->List
     (append* 
      (map List->list 
           (List->list
            (Operands Ls))))))
  
  (define (list->List l)
    (construct 'List l))
  
  (define List->list cdr)
  
  (define (List-ref L n)
    (if (and (List? L) (integer? n))
        (list-ref (List->list L) n)
        (construct 'List-ref (list L n))))
  
  (define (Map f L)
    ; The output of f must be a 
    ; simplified expression.
    (if (procedure? f)
        (list->List 
         (map f (List->list (Operands L))))
        (list->List 
         (map (λ (o) (list f o))
              (List->list (Operands L))))))
  
  (define (List->Set L)
    (Apply Set (Operands L)))
  
  (define (Variables u)
    (define (Vars u)
      (cond
        [(eq? (Kind u) 'symbolic-id)
         (List u)]
        [(atomic-expression? u) 
         (List)]
        [(or (plus-expression? u)
             (times-expression? u)
             (and (power-expression? u)
                  ; Mathematica considers only rational
                  ; powers. Check [Cohen].
                  (Rational? (exponent u))))
         (AppendStar (Map Vars (Operands u)))]
        [(compound-expression? u)
         (List u)]
        [else
         (List)]))
    (List->Set (Vars u)))
  
  (define (Sqrt u)
    (cond
      [(and (number? u) (negative? u))
       undefined]
      [(and (number? u) (integer? u))
       (define-values (s r) (integer-sqrt/remainder u))
       (if (zero? r) s (Power u 1/2))]
      [else (Power u 1/2)]))
  
  (define-syntax (define-listable stx)
    (syntax-case stx ()
      [(_ (name arg) body ...)
       #'(define (name arg)
           (if (List? arg)
               (Map name arg)
               (let () body ...)))]))  
  
  (define-syntax (Define stx)
    (syntax-case stx ()
      [(_ (var arg ...) val)
       #'(Define var (lambda (arg ...) val))]
      [(_ var val)
       (with-syntax 
           ([set!define 
             (if (identifier-binding #'var 0)                                                 
                 #'set! 
                 #'define)])
         #'(set!define var val))]))
  
  (define builtin-db
    (make-hash (list (cons Plus 'Plus)
                     (cons Minus 'Minus)
                     (cons Times 'Times)
                     (cons Quotient 'Quotient)
                     (cons Power 'Power))))
  
  (define (Apply f L)
    (if (and (procedure? f) (List? L))
        (apply f (List->list L))         
        (list->List               
         (let ([f (hash-ref builtin-db f f)])
           ; TODO: BUG HERE ...........................
           (construct f (Operands L))))))
  
  (define (List? u)
    (and (list? u)
         (eq? (Kind u) 'List)))

  (define-listable (Sqr u)
    (cond
      [(real? u) (sqr u u)]
      [else      (Power u 2)]))

  (define-syntax (define-real-function stx)
    (syntax-case stx ()
      [(_ new old)
       #'(begin
           (provide new)
           (define-listable (new u)
             (cond
               [(number? u) (old u)]
               [else (construct 'new (list u))])))]))
  
  (define-real-function Sin sin)
  (define-real-function Cos cos)
  (define-real-function Tan tan)
  (define-real-function Asin asin)
  (define-real-function Acos acos)
  (define-real-function Atan atan)
  (define-real-function Sinh sinh)
  (define-real-function Cosh cosh)
  (define-real-function Tanh tanh)
  ;(define-real-function Asinh asinh) ; in the Science collection
  ;(define-real-function Acosh acosh)
  ;(define-real-function Atanh atanh)
  (define-real-function Round round)
  (define-real-function Floor floor)
  (define-real-function Ceiling ceiling)
  (define-real-function Truncate truncate)
  (define-real-function Sgn sgn)
    
  (define (Solve-quadratic a b c)
    ; return List of all solutions to ax^2+bx+c=0
    (define d (Minus (Power b 2) (Times 4 a c)))
    (if (rational? d)
        (cond 
          [(< d 0) (List)]
          [(= d 0) (List (Quotient (Minus b) (Times 2 a)))]
          [else    (List (Quotient (Minus (Minus b) (Sqrt d)) (Times 2 a))
                         (Quotient (Plus  (Minus b) (Sqrt d)) (Times 2 a)))])
        ; Mathematica and NSpire pretends d>0 ...
        (List (Quotient (Minus (Minus b) (Sqrt d)) (Times 2 a))
              (Quotient (Plus  (Minus b) (Sqrt d)) (Times 2 a)))))
  
  (define (Solve-linear a b)
    ; return List of all solutions to ax+b=0
    (if (and (number? a) (not (zero? a)))
        (List (Minus (Quotient b a)))
        (List)))
  
  (define Range 
    (case-lambda
      [()              
       (list 'Range)]
      [(imax)
       (if (number? imax) (range imax) `(Range ,imax))]
      [(imin imax)
       (if (and (number? imin) (number? imax))
           (range imin imax)
           `(Range ,imin ,imax))]
      [(imin imax di)       
       (if (and (number? imin) 
                (number? imax)
                (number? di))
           (range imin imax di)
           `(Range ,imin ,imax ,di))]
      [args `(Range . ,args)]))
  
  (require "../adaptive-plotting.rkt")
  
  (define-match-expander List: 
    (λ (stx)
      (syntax-case stx ()
        [(_ pat ...) #'(list 'List pat ...)])))
  
  (define-namespace-anchor a)
  (define ns (namespace-anchor->namespace a))
  
  (define (N u)
    ; TODO: Improve this
    (eval u ns))
    
  (define (Plot f range [options '(List)])
    ; TODO: Implement options
    (displayln (list f range))
    (define y-min -5)
    (define y-max +5)
    (define excluded? #f)
    (match range
      [(List: var x-min x-max)
       (plot2d (if (procedure? f) 
                   (λ (x) (displayln x) (f x)) 
                   (λ (x) (N (Substitute f (Equal var x)))))
               x-min x-max y-min y-max excluded?)]
      [else (error)]))
    
  #;(and (real? x-min) (real? x-max) (real? y-min) (real? y-max)
         (< x-min x-max) (< y-min y-max))
  
;  (define (Monomial-gpe u v)
;    (define s (if (eq? (Kind v) 'set) (list v) v))
;    (cond
;      [(Member? u (operands s)) #t]
;      [else
;      (if(power-expression? u)
;         (define base (Operand u 0))
;         (define exponent (Operand u 1))
;         (if (and (Member? base s)
;                (eq? (Kind exponent) 'integer)
;                (> exponent 1))
  )
  

(module test racket
  (require (submod ".." symbolic-application)
           (submod ".." bracket)
           rackunit)
  (define x 'x)
  (define y 'y)
  (define z 'z)
  (define a 'a)
  (define b 'b)
  (define c 'c)
  (define d 'd)
  (define f 'f)
  
  (displayln "TEST - Running tests in mpl.rkt")
  
  ;;; Kind
  (check-equal? (Kind 1) 'integer)
  (check-equal? (Kind 1/2) 'rational)
  (check-equal? (Kind (sqrt 2)) 'real)
  (check-equal? (Kind 1+5i) 'complex)
  (check-equal? (Kind (Plus 1 x)) 'Plus)
  (check-equal? (Kind (Times 2 x)) 'Times)
  (check-equal? (Kind (Power 2 x)) 'Power)
  ;;; Plus
  (check-equal? (Plus 1 1) 2)
  (check-equal? (Plus x 0) x)
  (check-equal? (Plus 0 x) x)
  (check-equal? (Plus x 3 x) '(Plus 3 (Times 2 x)))
  (check-equal? (Plus x 3 (Times x 4)) '(Plus 3 (Times 5 x)))
  (check-equal? (Plus x 3 (Minus x)) 3)
  (check-equal? (Plus x 3 (Minus x) (Times x 5)) (Plus 3 (Times 5 x)))
  ;
  (check-equal? (Plus 1) 1)
  (check-equal? (Plus x) x)
  
  ;;; Times
  (check-equal? (Times 2 3) 6)
  (check-equal? (Times x 2) (Times 2 x))
  (check-equal? (Times 3 x 2) (Times 6 x))
  (check-equal? (Times 3 x 2 x) (Times 6 (Power x 2)))
  (check-equal? (Times 0 x 2 x) 0)
  (check-equal? (Times 1 x x) (Power x 2))
  ;;; Power
  (check-equal? (Power x 0) 1)
  (check-equal? (Power 0 0) 'undefined)
  (check-equal? (Power (Power x 2) 3) (Power x 6))
  ;;; Minus
  (check-equal? (Minus a b) (Plus a (Times -1 b)))
  ; ...
  ;;; Quotient and Power
  (check-equal? (Minus (Quotient (Times x y) 3))
                '(Times -1/3 x y))
  (check-equal? (Power (Power (Power x 1/2) 1/2) 8)
                '(Power x 2))
  (check-equal? (Power (Times (Power (Times x y) 1/2) (Power z 2)) 2)
                '(Times x y (Power z 4)))
  
  (check-equal? (Quotient x x) 1)
  (check-equal? (Times (Quotient x y) (Quotient y x)) 1)
  (check-equal? (Times 2 3) 6)
  (check-equal? (Times 2 x) '(Times 2 x))
  (check-equal? (Times z y x 2) '(Times 2 x y z))
  (check-equal? (Times (Power x 2) (Power x 3))
                '(Power x 5))
  (check-equal? (Plus x y x z 5 z)
                '(Plus 5 (Times 2 x) y (Times 2 z)))
  (check-equal? '(Times 1/2 x) (Quotient x 2))
  
  ; Threading of Plus, Times and Power
  (check-equal? (Plus (List 1 x)) (List 1 x))
  (check-equal? (Plus (List 1 2) (List 4 5)) (List 5 7))
  (check-equal? (Plus (List 1 x) 3) (List 4 (Plus x 3)))
  (check-equal? (Plus 3 (List 1 x)) (List 4 (Plus x 3)))
  (check-equal? (Plus y (List 1 x)) (List (Plus 1 y) (Plus x y)))
  (check-equal? (Times (List 1 x)) (List 1 x))
  (check-equal? (Times (List 1 2) (List 4 5)) (List 4 10))
  (check-equal? (Power (List 3 x) 2) (List 9 (Power x 2)))
  
  ;;; Substitute
  (check-equal? (Substitute (Plus a b) (Equal b x))
                (Plus a x))  
  (check-equal? (Substitute (Plus (Quotient 1 a) a) (Equal a x))
                (Plus (Power x -1) x))
  (check-equal? (Substitute (Plus (Power (Plus a b) 2) 1) (Equal (Plus a b) x))
                (Plus 1 (Power x 2)))
  (check-equal? (Substitute (Plus a b c) (Equal (Plus a b) x))
                (Plus a b c))
  (check-equal? (Substitute (Plus a b c) (Equal a (Minus x b)))
                (Plus c x))
  ;;; Sequential-substitute
  (check-equal? (Sequential-substitute 
                 (Plus x y)
                 (List (Equal x (Plus a 1))
                       (Equal y (Plus b 2))))
                (Plus 3 a b))
  (check-equal? (Sequential-substitute 
                 (Plus x y)
                 (List (Equal x (Plus a 1))
                       (Equal a (Plus b 2))))
                (Plus 3 b y))  
  (check-equal? (Sequential-substitute
                 (Equal (f x) (Plus (Times a x) b))
                 (List (Equal (f x) 2)
                       (Equal x 3)))
                (Equal 2 (Plus (Times 3 a) b)))  
  (check-equal? (Sequential-substitute 
                 (Equal (f x) (Plus (Times a x) b))
                 (List (Equal x 3)
                       (Equal (f x) 2)))
                (Equal (f 3) (Plus (Times 3 a) b)))
  ;;; Concurrent-substitute
  (check-equal? (Concurrent-substitute 
                 (Times (Plus a b) x)
                 (List (Equal (Plus a b) (Plus x c))
                       (Equal x d)))
                (Times d (Plus c x)))
  (check-equal? (Concurrent-substitute 
                 (Equal (f x) (Plus (Times a x) b))
                 (List (Equal x 3)
                       (Equal (f x) 2)))
                (Equal 2 (Plus (Times 3 a) b)))
  ; Expand
  (check-equal? (Expand (Power (Plus a b) 2))
                (Plus (Power a 2) (Times 2 a b) (Power b 2)))
  (check-equal? (Expand (Times a (Plus x y)))
                (Plus (Times a x) (Times a y)))
  
  
  )


#;(require (submod "." symbolic-application)
           (submod "." mpl))

;(define x 'x)
;(define y 'y)
;(define z 'z)
;(define a 'a)
;(define b 'b)
;(define c 'c)
;(define d 'd)
;(define f 'f)

;;; Problem: (Power (Plus 1 y) 2) is not expanded
; > (Algebraic-expand (Power (Plus (Times x (Power (Plus y 1) 1/2)) 1) 4))
; '(Plus 1 (Times 4 x) (Times 6 (Power x 2)) (Times 4 (Power x 3)) (Times (Power x 4) (Power (Plus 1 y) 2)))
