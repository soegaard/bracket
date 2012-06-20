#lang racket
(provide (rename-out 
          [bracket-read read]
          [bracket-read-syntax read-syntax]))

(require "parser.rkt")
(require syntax/strip-context)

(define (bracket-read in)
  (syntax->datum
   (bracket-read-syntax #'from-my-read in)))

(define (bracket-read-syntax src in)
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
                       (build-path base "../bracket-lang.rkt"))]
                    [bracket.rkt
                     (let ()
                       (define-values (base file _)
                         (split-path
                          (resolved-module-path-name
                           (module-path-index-resolve
                            (syntax-source-module #'here)))))
                       (build-path base "../bracket.rkt"))])
        (syntax-property 
         (strip-context   
          #'(module anything bracket-lang
              (require (submod bracket.rkt bracket)
                       (submod bracket.rkt symbolic-application))
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
