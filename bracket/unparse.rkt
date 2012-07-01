#lang racket
;;;
;;; Unparse: From prefix to infix representation.
;;;

(provide unparse)

(require (submod "bracket.rkt" expression-core))

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
  (case (operator form)
    [(Plus) 
     (string-append* 
      (maybe-add-between 
       (map unparse-product (operands form))
       "+" #\-))]
    [else
     (unparse-product form first? level-below-times?)]))

(define (wrap-if test str)
  (if test (string-append "(" str ")") str))

(define (unparse-product form [first? #t] [level-below-times? #f])
  (case (operator form)
    [(Times) 
     (define ops (operands form))
     (define unwrapped
       (string-append* (add-between (map unparse-factor ops) "*")))
     (wrap-if (and level-below-times? (>= (length ops) 2))
              unwrapped)]
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
    [(or (compound-expression? form)
         (set-expression? form))
     (format "~a(~a)" (operator form)
             (string-append* (add-between (map/first? #t unparse (operands form)) ",")))]
    [(plus-expression? form)
     (format "(~a)" (unparse-sum form #t))]
    [(times-expression? form)
     (format "(~a)" (unparse-product form #t))]
    [else (error 'unparse-factor "Internal Bracket Error, got " form)]))

(define (unparse-power form)
  (case (operator form)
    [(Power)
     (define b (base form))
     (define e (exponent form))
     (define bi (unparse b #f #t))
     (define ei (unparse e #f #t))
     (format "~a^~a" bi ei)]
    [else (error)]))


(module* test #f
  (require rackunit )
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
  ; Products
  (check-equal? (unparse '(Times 2 x)) "2*x")
  (check-equal? (unparse '(Times 2/3 x)) "2/3*x")
  (check-equal? (unparse '(Times -2 x)) "-2*x")
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
  )
