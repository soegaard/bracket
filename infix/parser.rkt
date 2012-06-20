#lang scheme
(require "parameter.ss")

;;;
;;; NOTES: 
;;;   Changes from planet version:  ]] (CDB) Bug fixed
;;;   Strings are supported.
;;;   Assignments changed from set! to define.
;;;   Assignment of functions.
;;;   Bug fix:  identifiers with more than one _ wasn't converted correctly

; <e> = <num>
;      |  <id>                   variable reference
;      |  <e> [ <args> ]         application
;      |  { <args> }             list construction
;      |  <e> + <e>              addition
;      |  <e> - <e>              subtraction
;      |  <e> * <e>              multiplication
;      |  <e> / <e>              division
;      |  <e> ^ <e>              exponentiation
;      |  - <e>                  negation
;      | ( <e> )                 grouping

; <id>   An identifier begins with a letter, 
;        and is optionally followed by series of letters, digits or underscores.
;        An underscore is converted to a -. Thus list_ref will refer to list-ref.

; <num>  A number is an non-empty series of digits, 
;        optionally followed by a period followed by a series of digits.

; <string> A number is a " followed by a series of non-" characters followed by a " .

(provide parse-expression parse-expression-from-port parse-math-string)

(require parser-tools/yacc
         parser-tools/lex
         (prefix-in : parser-tools/lex-sre)
         syntax/readerr)

(define-tokens value-tokens (NUMBER IDENTIFIER STRING))
(define-empty-tokens op-tokens (newline := 
                                        OP CP    ; ( ) 
                                        OB CB    ; [ ]
                                        OC CC    ; { }
                                        ODB   ; [[ ]]
                                        COMMA    ; ,
                                        SEMI     ; ;
                                        PERIOD   ; .
                                        LAMBDA   ; lambda or λ
                                        SQRT     ; √ 
                                        NEG      ; ¬  (logical negation)
                                        LESS-EQUAL    ; <= or ≤
                                        GREATER-EQUAL ; >= or ≥
                                        NOT-EQUAL     ; <> or ≠
                                        = < >
                                        + - * / ^ 
                                        EOF))

(define-lex-abbrevs
  [letter     (:or (:/ "a" "z") (:/ #\A #\Z) )]
  [digit      (:/ #\0 #\9)]
  [string     (:: #\" (:* (:or letter digit #\_ #\?)) #\")]
  [identifier (:: letter (:* (:or letter digit #\_ #\?)))])

(define expression-lexer
  (lexer-src-pos
   [(eof) 'EOF]
   [(:or #\tab #\space #\newline)    ; this skips whitespace
    (return-without-pos (expression-lexer input-port))] 
   [#\newline (token-newline)]  ; (token-newline) returns 'newline
   [(:or ":=" "+" "-" "*" "/" "^" "<" ">" "=" "\"") (string->symbol lexeme)] 
   ["(" 'OP]
   [")" 'CP]
   ["[" 'OB]
   ["]" 'CB]
   ["{" 'OC]
   ["}" 'CC]
   ["[[" 'ODB]
   ; ["]]" 'CDB]
   ["," 'COMMA]   
   [";" 'SEMI]
   ["." 'PERIOD]
   [#\λ 'LAMBDA]
   ["lambda" 'LAMBDA]
   ["√" 'SQRT]
   ["¬" 'NEG]
   ["≤" 'LESS-EQUAL]
   ["<=" 'LESS-EQUAL]
   ["≥" 'GREATER-EQUAL]
   [">=" 'GREATER-EQUAL]
   ["<>" 'NOT-EQUAL]
   ["≠" 'NOT-EQUAL]
   [string 
    (token-STRING (substring lexeme 1 (- (string-length lexeme) 1)))]
   [identifier 
    (token-IDENTIFIER (string->symbol (regexp-replace* #rx"_" lexeme "-")))]
   [(:+ digit) (token-NUMBER (string->number lexeme))]
   [(:: (:+ digit) #\. (:* digit)) (token-NUMBER (string->number lexeme))]))


;; A macro to build the syntax object
(define-syntax (b stx)
  (syntax-case stx ()
    ((_ o value start end)
     (with-syntax 
         ((start-pos (datum->syntax #'start
                                    (string->symbol 
                                     (format "$~a-start-pos"
                                             (syntax->datum #'start)))))
          (end-pos (datum->syntax #'end
                                  (string->symbol 
                                   (format "$~a-end-pos" 
                                           (syntax->datum #'end))))))
       #`(datum->syntax o
                        value
                        (list (if (syntax? o) (syntax-source o) 'missing-in-action--sorry)
                              (if o (+ (syntax-line o) (position-line start-pos) -1) #f)
                              (if o (+ (syntax-column o) (position-offset start-pos) ) #f)
                              (if o (+ (syntax-position o) (position-offset start-pos)) #f)
                              (- (position-offset end-pos)
                                 (position-offset start-pos)))
                        o o)))))

; for testing: builds lists instead of syntax objects
#;(define-syntax (b stx)
    (syntax-case stx ()
      [(_ _ val _ _)
       #'val]))

(define (display-position-token pos)
  (display (list (position-offset pos)
                 (position-line pos)
                 (position-col pos))))

(define (expression-parser source-name orig-stx)
  (define o orig-stx)
  (parser
   (src-pos)
   (suppress)  ; hmm...
   (start start)
   (end newline EOF)
   (tokens value-tokens op-tokens)
   (error (lambda (token-ok? name val start end)
            ; The first argument will be #f if and only if the error is that an invalid token was received. 
            ; The second and third arguments will be the name and the value of the token at which the error was detected. 
            ; The fourth and fifth arguments, if present, provide the source positions of that token.
            (unless #f #; (string? (syntax->datum o))
              (display "DEBUG: ")
              (display (list o token-ok? name val start end))
              (display-position-token start) (newline)
              (display-position-token end) (newline)
              (newline))
            (raise-syntax-error 
             'expression-parser "parse error" o
             (datum->syntax 
              o 
              (if (string? (syntax->datum o))
                  (substring (syntax->datum o)
                             (max 0 (- (position-offset start) 1))
                             (min (- (position-offset end) 1)
                                  (string-length (syntax->datum o))))
                  (begin
                    (display o)
                    (newline)
                    "fooooooooo"))
              (list (if (syntax? o) (syntax-source o) 'missing-in-action--sorry)
                    (if o (+ (syntax-line o)     (position-line start) -1) #f)
                    (if o (+ (syntax-column o)   (position-offset start))  #f)
                    (if o (+ (syntax-position o) (position-offset start))  #f)
                    (- (position-offset end)
                       (position-offset start)))))))
   
   (precs (right :=)
          (left - +)
          (left * /)
          (right OB)
          (right ^)
          (left =)  ; comparisons
          (right NEG)
          (left SEMI))
   
   (grammar    
    (start [(exp) (b o `(#%infix ,$1) 1 1)] 
           [() #f])
    ;; If there is an error, ignore everything before the error
    ;; and try to start over right after the error    
    
    (args [(exp)            (b o (list $1) 1 1)]
          [(exp COMMA args) (b o (cons $1 $3) 1 3)]
          [() '()])
    
    (ids [()               '()]
         [(IDENTIFIER ids) (b o (cons $1 $2) 1 2)])
        
    (parenthensis-exp
     [(OP exp CP)                                   $2])
    
    (atom 
     [(NUMBER)                                      (b o $1 1 1)]
     [(IDENTIFIER)                                  (b o $1 1 1)]
     [(STRING)                                      (b o $1 1 1)]
     [(parenthensis-exp)                            $1])
    
    (construction-exp
     [(OC args CC)                                  (b o `(,(b o 'list 1 3) ,@$2) 1 3)]         
     [(OP LAMBDA ids PERIOD exp CP)                 (b o `(,(b o 'lambda 2 2) ,$3 ,$5) 1 6)]
     [(atom)                                        $1])
    
    (application-exp
     [(application-exp OB args CB)                              (b o `(,$1 ,@$3) 1 4)]                     ; function application
     [(application-exp ODB exp CB CB)                             (b o `(,(b o 'list-ref 1 4) ,$1 ,$3) 1 4)] ; list ref
     [(construction-exp)                            $1])

    #;(implicit-exp
       [(application-exp application-exp)   (prec *)    (b o `(,(b o '* 1 2) ,$1 ,$2) 1 2)]    ; implicit
       [(application-exp)                            $1])
    
    (power-exp 
     [(application-exp ^ power-exp) (prec ^)           (b o `(expt ,$1 ,$3) 1 3)]
     [(application-exp)                                $1])
    
    (sqrt-exp
     [(SQRT sqrt-exp)                               (b o `(,(b o 'sqrt 1 1) ,$2) 1 2)]
     [(power-exp)                                   $1])
    
    (negation-exp 
     [(- negation-exp)                              (b o `(,(b o '- 1 1) ,$2) 1 2)]
     [(sqrt-exp)                                    $1])
    
    (multiplication-exp
     [(multiplication-exp * negation-exp) (prec *)  (b o `(,(b o '* 2 2) ,$1 ,$3) 1 3)]
     [(multiplication-exp / negation-exp) (prec /)  (b o `(,(b o '/ 2 2) ,$1 ,$3) 1 3)]
     ;[(multiplication-exp  negation-exp) (prec *)  (b o `(,(b o '* 1 2) ,$1 ,$2) 1 2)]
     [(negation-exp)                                $1])
     
    (addition-exp
     [(addition-exp - multiplication-exp) (prec -)  (b o `(,(b o '- 2 2) ,$1 ,$3) 1 3)]
     [(addition-exp + multiplication-exp) (prec +)  (b o `(,(b o '+ 2 2) ,$1 ,$3) 1 3)]
     [(multiplication-exp)                          $1])
    
    (order-exp
     [(addition-exp LESS-EQUAL addition-exp)    (prec =)  (b o `(,(b o '<= 2 2) ,$1 ,$3) 1 3)]
     [(addition-exp < addition-exp)             (prec =)  (b o `(,(b o '< 2 2) ,$1 ,$3) 1 3)]
     [(addition-exp GREATER-EQUAL addition-exp) (prec =)  (b o `(,(b o '>= 2 2) ,$1 ,$3) 1 3)]
     [(addition-exp > addition-exp)             (prec =)  (b o `(,(b o '> 2 2) ,$1 ,$3) 1 3)]
     [(addition-exp NOT-EQUAL addition-exp)     (prec =)  (b o `(not (,(b o '= 2 2) ,$1 ,$3)) 1 3)]
     [(addition-exp = addition-exp)             (prec =)  (b o `(,(b o '= 2 2) ,$1 ,$3) 1 3)]
     [(addition-exp)                            $1])
    
    (logical-negation-exp
     [(NEG logical-negation-exp)   (prec NEG)      (b o `(,(b o 'not 1 1) ,$2) 1 2)]
     [(order-exp)                                  $1])
    
    (assignment-exp
     [(IDENTIFIER := assignment-exp)                  (b o `(,(b o 'define 2 2) ,$1 ,$3) 1 3)]
     [(IDENTIFIER OP IDENTIFIER CP := assignment-exp) (b o `(,(b o 'define 3 3) (,$1 ,$3) ,$6) 1 6)]
     [(logical-negation-exp)                          $1])
    
    (compound-exp 
     [(compound-exp SEMI assignment-exp)            (b o `(,(b o 'begin 2 2) ,$1 ,$3) 1 3)]
     [(assignment-exp)                              $1])
     
    (exp 
     [(compound-exp)                                $1]))))

;; run the calculator on the given input-port       
(define (parse-expression-from-port ip)
  (port-count-lines! ip)
  (letrec ((one-line
            (lambda ()
              (let ((result ((expression-parser "test" #f) 
                             (λ () (expression-lexer ip)))))
                (when result
                  (printf "~a~n" result)
                  (one-line))))))
    (one-line)))

(define (parse-expression stx ip)
  (port-count-lines! ip)
  ((expression-parser stx stx) (λ () (expression-lexer ip))))

(define parse-math-string
  (case-lambda 
    [(s)     
     (display (format "~a\n" s))
     (parse-math-string s (let ([here #'here]) (datum->syntax here s here)))]
    [(s src) 
     (cond 
       [(string? s)
        (parse-expression src (open-input-string s))]
       [(special-comment? s)
        s]
       [else
        (if (or (symbol? s) (boolean? s))
            s
            (datum->syntax (second s) (cons 'quote-syntax (cdr s))))])]))
