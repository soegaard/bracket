#lang racket
;;;
;;; Unparse: From prefix to infix representation.
;;;

(provide unparse)

(require (submod "bracket.rkt" expression-core)
         (submod "bracket.rkt" equation-expression)
         (submod "bracket.rkt" bracket)
         slideshow/pict)


(define (map/first? base f xs)
  (cond
    [(empty? xs) '()]
    [(empty? (rest xs)) (list (f (first xs) base))]
    [else 
     (cons (f (first xs) base)
           (map (λ (x) (f x #f)) (rest xs)))]))

(define (maybe-add-between xs b c)
  (cond
    [(empty? xs) '()]
    [(empty? (rest xs)) xs]
    [(if (eqv? (string-ref (second xs) 0) c)
         (cons (first xs)
               (maybe-add-between (rest xs) b c))
         (cons (first xs)
               (cons b
                     (maybe-add-between (rest xs) b c))))]))

(define (unparse form [first? #t] [level-below-times? #f])
  (unparse-sum form first? level-below-times?))

(define (unparse-sum form [first? #t] [level-below-times? #f])
  (if (pict? form)
      form ; hack: A single pict is returned as is
           ; The unparse functions needs to return
           ; something else than just strings in order
           ; support other values such as snips.
      (case (operator form)
        [(Plus)
         (define ops (operands form))
         (define unwrapped
           (string-append* 
            (maybe-add-between (map unparse-product ops) "+" #\-)))
         (wrap-if (and level-below-times? (>= (length ops) 2))
                  unwrapped)]
        [else
         (unparse-product form first? level-below-times?)])))

(define (wrap-if test str)
  (if test (string-append "(" str ")") str))

(define (unparse-product form [first? #t] [level-below-times? #f])
  (case (operator form)
    [(Times) 
     (define ops (operands form))
     (cond
       [(empty? ops) "1"]
       [(eqv? (first ops) -1)
        (string-append "-" (unparse-product (cons 'Times (rest ops))))]
       [else
        (define-values (den num)  
          (partition (λ (f) (and (power-expression? f)
                                 (number? (exponent f))
                                 (negative? (exponent f))))
                     ops))
        (set! den (map (λ (f) (Power (base f) (- (exponent f)))) den))
        (define (format-factors ops)
          (define unwrapped
            (string-append* (add-between (map unparse-factor ops) "*")))
          (wrap-if (and level-below-times? (>= (length ops) 2))
                   unwrapped))
        (if (empty? den)
            (format-factors num)
            (format "~a/~a" 
                    (format-factors num) 
                    (wrap-if (>= (length den) 2)
                             (unparse (apply Times den)))))])]
    [else
     (unparse-factor form first?)]))

(define (unparse-number form [first? #t])
  (define num-str (number->string form))
  (define (wrap) (string-append "(" num-str ")"))    
  (cond
    [first? num-str]
    [(negative? form) (wrap)]
    [(and (exact? form) (not (integer? form))) (wrap)]
    [else num-str]))

(define (unparse-factor form [first? #t])
  (cond
    [(power-expression? form)
     (unparse-power form)]
    [(number? form)
     (unparse-number form first?)]
    [(symbol? form)
     (symbol->string form)]
    [(list-expression? form)
     (format "{~a}" 
             (string-append* (add-between (map/first? #t unparse (operands form)) ",")))]
    [(plus-expression? form)
     (format "(~a)" (unparse-sum form #t))]
    [(times-expression? form) ; This case is for unsimplified expressions
     (format "(~a)" (unparse-product form #t))]
    [(compound-expression? form)
     (case (kind form)
       [(Equal)
        (define-values (t r) (equation->sides form))
        (format "~a=~a" (unparse t) (unparse r))]
       [else
        ; Note: Set expressions are compound expressions.
        (format "~a(~a)" (operator form)
                (string-append* (add-between (map/first? #t unparse (operands form)) ",")))])]
    [(eq? form #t)
     "true"]
    [(eq? form #f)
     "false"]
    [else
     ; TODO: pass value unchanged: stuff like #void, #eof, special values etc.
     (format "~a" (object-name form))
     #;(format form)]))

(define (unparse-power form)
  (case (operator form)
    [(Power)
     (define b (base form))
     (define e (exponent form))
     (define bi (unparse b #f #t))
     (define ei (unparse e #f #t))
     (format "~a^~a" bi ei)]
    [else (error 'unparse-power "Internal Bracket Error: got non-power: ~a" form)]))


(module* test #f
  (require rackunit )
  (require (submod "bracket.rkt" bracket))
  (define x 'x)
  (define y 'y)
  (define z 'z)
  
  (displayln "TEST - Running tests in unparse.rkt")
  
  ;;; Numbers
  (check-equal? (unparse 1) "1")
  (check-equal? (unparse 1 #f) "1")
  (check-equal? (unparse -1) "-1")
  (check-equal? (unparse -1 #f) "(-1)")
  (check-equal? (unparse 2/3) "2/3")
  (check-equal? (unparse 2/3 #f) "(2/3)")
  (check-equal? (unparse -2/3) "-2/3")
  (check-equal? (unparse -2/3 #f) "(-2/3)")
  
  ;;; Variables
  (check-equal? (unparse x) "x")
  ; Sums
  (check-equal? (unparse '(Plus 1 x)) "1+x")
  (check-equal? (unparse '(Plus -1 x)) "-1+x")
  (check-equal? (unparse '(Plus 2/3 x)) "2/3+x")
  (check-equal? (unparse '(Plus 2.0 x)) "2.0+x")
  (check-equal? (unparse (Minus x)) "-x")
  ; Products
  (check-equal? (unparse '(Times 2 x)) "2*x")
  (check-equal? (unparse '(Times 2/3 x)) "2/3*x")
  (check-equal? (unparse '(Times -2 x)) "-2*x")
  ; Products of products (unsimplified)
  (check-equal? (unparse '(Times 2 (Times 3 x) (Times -5 y))) "2*(3*x)*(-5*y)")
  ; Products with factors with negative exponents
  (check-equal? (unparse (Quotient x y)) "x/y")
  (check-equal? (unparse (Quotient x (Times y z))) "x/(y*z)")
  ; Powers
  (check-equal? (unparse '(Power 2 3)) "2^3")
  (check-equal? (unparse '(Power -2 3)) "(-2)^3")
  (check-equal? (unparse '(Power 2 -3)) "2^(-3)")
  (check-equal? (unparse '(Power -2 -3)) "(-2)^(-3)")
  (check-equal? (unparse '(Power 2/3 4)) "(2/3)^4")
  (check-equal? (unparse '(Power 2/3 4/5)) "(2/3)^(4/5)")
  (check-equal? (unparse '(Power 2 4/5)) "2^(4/5)")
  ; Powers of products
  (check-equal? (unparse '(Power (Times x y) 3)) "(x*y)^3")
  (check-equal? (unparse '(Power (Times x y) 3)) "(x*y)^3")
  ; Powers of sums
  (check-equal? (unparse '(Power (Plus x 3) 7)) "(x+3)^7")
  ; Sums of products
  (check-equal? (unparse '(Plus  2 (Times 3 x)  (Times  4 y)))  "2+3*x+4*y")
  (check-equal? (unparse '(Plus  2 (Times 3 x)  (Times -4 y)))  "2+3*x-4*y")
  (check-equal? (unparse '(Plus  2 (Times -3 x) (Times  4 y)))  "2-3*x+4*y")
  (check-equal? (unparse '(Plus    (Times -3 x) (Times  4 y)))  "-3*x+4*y")
  (check-equal? (unparse '(Plus -3              (Times  4 y)))  "-3+4*y")
  (check-equal? (unparse '(Plus  2 (Times -3 x) (Times -4 y))) "2-3*x-4*y")
  ; compound expressions
  (check-equal? (unparse '(Sin x))  "Sin(x)")
  (check-equal? (unparse '(Sin (Plus (Times -3 x))))  "Sin(-3*x)")
  (check-equal? (unparse '(Times -2 (Sin x)))  "-2*Sin(x)")
  (check-equal? (unparse '(Times -2 (Sin x) (Cos y)))  "-2*Sin(x)*Cos(y)")
  (check-equal? (unparse '(Power (Times -2 (Sin x)) 5))  "(-2*Sin(x))^5")
  ; A sum as factor
  (check-equal? (unparse '(Times x (Plus -9 (Power x 2)))) "x*(-9+x^2)")
  ; List
  (check-equal? (unparse '(List)) "{}")
  (check-equal? (unparse '(List 1)) "{1}")
  (check-equal? (unparse '(List 1 2)) "{1,2}")
  (check-equal? (unparse '(List 1 2 3)) "{1,2,3}")
  ; Set
  (check-equal? (unparse '(Set)) "Set()")
  (check-equal? (unparse '(Set 1)) "Set(1)")
  (check-equal? (unparse '(Set 1 2)) "Set(1,2)")
  (check-equal? (unparse '(Set 1 2 3)) "Set(1,2,3)")
  ; Equal
  (check-equal? (unparse '(Equal x 2)) "x=2")
  ; Booleans
  (check-equal? (unparse #t) "true")
  (check-equal? (unparse #f) "false")
  )
