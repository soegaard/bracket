#lang racket
(provide (rename-out 
          [my-read read]
          [my-read-syntax read-syntax]))

(require "parser.rkt")
(require syntax/strip-context)

(define (my-read in)
  (syntax->datum
   (my-read-syntax #'from-my-read in)))

(define (my-read-syntax src in)
  (if (eof-object? (peek-byte in))
      eof
      (with-syntax ([body (parse-expression 
                           src
                           #'from-my-read-syntax in)])
        (syntax-property 
         (strip-context   
          #'(module anything "bracket-lang.rkt"
              (define-syntax (#%infix stx)
                ;(displayln (list 'my-read-syntax: stx))
                (syntax-case stx ()
                  [(_ expr) #'expr]))              
              (require (submod "bracket.rkt" bracket)
                       (submod "bracket.rkt" symbolic-application))
              ; This lists the operators used by the parser.
              (define expt Power)
              (define + Plus)
              (define - Minus)
              (define * Times)
              (define / Quotient)
              (define = Equal)
              (define sqrt Sqrt)
              (define list List)
              (define list-ref List-ref)
              (define-syntax (define stx)
                (syntax-case stx () [(_ . more) #'(Define . more)]))
              body))
         'module-language
         '#(bracket/bracket-info get-language-info #f)))))

(define (get-info in mod line col pos)
  (lambda (key default)
    (case key
      [(color-lexer)
       (dynamic-require 'syntax-color/default-lexer
                        'default-lexer)]
      [else default])))
             