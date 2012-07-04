#lang racket
(provide (rename-out 
          [bracket-read read]
          [bracket-read-syntax read-syntax]
          [bracket-read-expression-syntax read-expression-syntax])
         get-info)

(require "parser.rkt")
(require syntax/strip-context
         racket/syntax)

(define (bracket-read in)
  (syntax->datum
   (bracket-read-syntax #'from-my-read in)))

(define (bracket-read-expression-syntax src in)
  (if (eof-object? (peek-byte-or-special in))
      eof
      (with-syntax ([body (parse-expression 
                           src
                           #'from-my-read-syntax in)])
        (strip-context #'body))))

(define (bracket-read-syntax src in)
  (define out
  (if (eof-object? (peek-byte in))
      eof
      (with-syntax ([body (parse-expression 
                           src
                           #'from-my-read-syntax in)]
                    [bracket-lang
                     (let ()
                       (define-values (base file _)
                         (split-path
                          (resolved-module-path-name
                           (module-path-index-resolve
                            (syntax-source-module #'here)))))
                       (path->string 
                        (simplify-path
                         (build-path base "../bracket-lang.rkt"))))]
                    [bracket.rkt
                     (let ()
                       (define-values (base file _)
                         (split-path
                          (resolved-module-path-name
                           (module-path-index-resolve
                            (syntax-source-module #'here)))))
                       (path->string 
                        (simplify-path
                         (build-path base "../bracket.rkt"))))]
                    [module-name (generate-temporary "main")])
        (syntax-property 
         (strip-context ; reason: see read docs on read-syntax
          #'(module module-name bracket/bracket-lang
              (require (submod (file bracket.rkt) bracket)
                       (submod (file bracket.rkt) symbolic-application))
              (define-syntax (#%infix stx)
                (syntax-case stx () [(_ expr) #'expr]))              
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
              (require bracket/lang/parser)              
              #;(current-read-interaction
                 (λ (_ in)
                   (parse-expression 'repl #'repl in)))
              body))
         'module-language
         '#(bracket/bracket-info get-language-info #f)))))
  ; DEBUG This line displays the syntax object returned by the reader.
  ; (write out) (newline)
  out)

(define (get-info in mod line col pos)
  (lambda (key default)
    (case key
      [(color-lexer)
       (dynamic-require 'bracket/lang/parser 'color-lexer)]
      [else default])))
