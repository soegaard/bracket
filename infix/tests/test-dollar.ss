#lang at-exp scheme
(require "../main.rkt"
         (planet schematics/schemeunit:3:4))

; <e> :== <num>
;      |  <id>                   variable reference
;      |  <e> [ <args> ]         application
;      |  <e> [[ <e> ]]          list reference
;      |  { <args> }             list construction
;      |  (λ <ids> . <e>)        lambda

;      |  <e> + <e>              addition
;      |  <e> - <e>              subtraction
;      |  <e> * <e>              multiplication
;      |  <e> / <e>              division
;      |  <e> ^ <e>              exponentiation
;      |  - <e>                  negation
;      |  √ <e>                  square root
;      | ( <e> )                 grouping
;      | <id> := <e>             assignment
;      | <e> ; <e>               compound

; <args> :== 
;         |   <e>
;         |   <e>, <args>+

; <id>   An identifier begins with a letter, 
;        and is optionally followed by series of letters, digits or
;        underscores.
;        An underscore is read as a hyphen. 
;        Thus list_ref will refer to list-ref.

; <ids>  A, possible empty, list of identifiers separated by white space.

; <num>  A number is an non-empty series of digits, 
;        optionally followed by a period followed by a series of digits.

(test-case
 "numbers"
 (check-equal? @${0} 0)
 (check-equal? @${123} 123)
 (check-equal? @${12.34}12.34))

(test-case
 "identifiers"
 (let ([x 0] [x1 1] [x12 12] [x-y 42])
   (check-equal? @${x}0)
   (check-equal? @${x1}1)
   (check-equal? @${x12}12)
   (check-equal? @${x_y}42)))

(test-case
 "application  expr[arg,...]"
 (check-equal? @${sin[0]}0)
 (check-equal? @${quotient[7,2]}3)
 (let ([thunk (lambda () 42)])
   (check-equal? @${thunk[]} 42)))

(test-case
 "list reference syntax  list[[index]]"
 (let ([xs (list 1 2 3)])
   (check-equal? @${xs[[0]]} 1)
   (check-equal? @${xs[[1]]} 2)
   (check-equal? @${xs[[2]]} 3)))


(test-case
 "anonymous function syntax  (λ ids . expr)"
 (check-equal? @${(λ.1)[]} 1)
 (check-equal? @${(λx.x+1)[2]} 3)
 (check-equal? @${(λx y.x+y+1)[1,2]} 4))



(test-case
 "list construction syntax {}"
 (check-equal? @${{}} '())
 (check-equal? @${{1}} '(1))
 (check-equal? @${{1,2}} '(1 2))
 (check-equal? @${{1,2,3}} '(1 2 3)))

(test-case
 "addition + with 2 or more arguments"
 (check-equal? @${1+2} 3)
 (check-equal? @${1+2+3} 6)
 (check-equal? @${1+2+3+4} 10))

(test-case
 "subtraction - with 2 or more arguments"
 (check-equal? @${1-2} -1)
 (check-equal? @${1-2-3} -4)
 (check-equal? @${1-2-3-4} -8))

(test-case
 "multiplication * with 2 or more arguments"
 (check-equal? @${1*2} 2)
 (check-equal? @${1*2*3} 6)
 (check-equal? @${1*2*3*4} 24))

(test-case
 "division / with 2 or more arguments"
 (check-equal? @${120/2} 60)
 (check-equal? @${120/2/3} 20)
 (check-equal? @${120/2/3/4} 5))

(test-case
 "exponentiation ^ with 2 or more arguments"
 (check-equal? @${2^3} 8)
 (check-equal? @${2^3^4} (expt 2 (expt 3 4))))

(test-case
 "negation, i.e. unary -"
 (check-equal? @${-1} -1)
 (check-equal? @${1+-2} -1)
 (check-equal? @${1--2} 3))

(test-case
 "square root"
 (check-equal? @${√4} 2)
 (check-equal? @${√(2+2)} 2)
 (check-equal? @${√4*√4} 4)
 (check-equal? @${√4*4} 8))

(test-case
 "comparisons"
 (check-equal? @${1<2} #t)
 (check-equal? @${1<=2} #t)
 (check-equal? @${1≤2} #t)
 (check-equal? @${1>2} #f)
 (check-equal? @${1>=2} #f)
 (check-equal? @${1≥2} #f)
 (check-equal? @${1<>2} #t)
 
 (check-equal? @${2<1} #f)
 (check-equal? @${2<=1} #f)
 (check-equal? @${2≤1} #f)
 (check-equal? @${2>1} #t)
 (check-equal? @${2>=1} #t)
 (check-equal? @${2≥1} #t)
 (check-equal? @${2<=1} #f)

 (check-equal? @${1<1} #f)
 (check-equal? @${1<=1} #t)
 (check-equal? @${1≤1} #t)
 (check-equal? @${1>1} #f)
 (check-equal? @${1>=1} #t)
 (check-equal? @${1≥1} #t)
 (check-equal? @${1<=1} #t)
 )

(test-case
 "logical negation"
 (let ([t #t] [f #f])
   (check-equal? @${¬t} #f)
   (check-equal? @${¬f} #t)
   (check-equal? @${¬1} #f)))


; TODO
#;(test-case
 "assignment  id := expr"
 (let ([x 0])
   (check-equal? (begin @${x:=1} x) 1)))

(test-case
 "compound   expr ; expr"
 (let ([x 0] [y 3])
   (check-equal? (begin @${(x:=1);(x+3)}) 4)
   (check-equal? (begin @${(x:=1);x+3}) 4)
   (check-equal? (begin @${x:=1;(x+3)}) 4)
   (check-equal? (begin @${x:=1;x}) 1)
   (check-equal? (begin @${x:=1;x;y:=x+7;y}) 8)))

(test-case
 "grouping"
 (check-equal? @${2*(3+4)} 14)
 (check-equal? @${(3+4)*2} 14)
 (check-equal? @${5*(3+4)*2} 70))

(test-case
 "precedence of + and *"
 (check-equal? @${1+2*3} 7)
 (check-equal? @${2*3+4} 10)
 (check-equal? @${2+3*4+5} 19))

#;(do comments still work? @${1+2})

(test-case
 "quoting"
 (check-equal? @$quote{1+2} '(#%infix (+ 1 2))))

(test-case
 "syntax-quoting"
 (check-equal? (syntax->datum @$quote-syntax{1+2}) '(#%infix (+ 1 2))))

(test-case
 "under score"
 (check-equal? @${list_ref[{1,2,3}, 0]} 1))

(require scheme/stxparam)

(test-case
 "#%infix"
 (check-equal? (syntax-parameterize ((#%infix (λ (x) #'42))) @${1+2}) 42)
 (check-equal?   
  (syntax-parameterize ([#%infix (syntax-rules () [(_ e) (+ e 1)])]) @${2})
  3))

(test-case
 "rebound +"
 (check-equal? (let ([+ -]) @${1+2}) -1))

(test-case
 "precedence"
 (check-equal? @${1+cos[0]} 2))

(test-case
 "function definition"
 (check-equal? (let ([x 1]) @${f(x):=x+1} (f 1)) 2))

(test-case
 "strings"
 (check-equal? @${"foo"} "foo"))



