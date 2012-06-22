#lang racket
 
(provide get-language-info get-info)

(define (get-language-info data)
  (lambda (key default)
    (displayln (list 'bracket-info/get-language-info key default))
    (case key
      [(configure-runtime)
       '(#(bracket/runtime-config configure #f))]
      [(color-lexer)       
       (dynamic-require 'syntax-color/default-lexer
                        'default-lexer)
       (displayln "HErE")
       (dynamic-require "parser.rkt"
                        'color-expression-lexer)]
      [else default])))

(require racket/runtime-path)
(define-runtime-path color-lexer-path "parser.rkt")

(define (get-info in mod line col pos)
  (displayln (list 'bracket-info/get-info in mod line col pos))
  (lambda (key default)
    (displayln (list  'reader/get-info  key default))
    (case key
      [(color-lexer)
       (dynamic-require color-lexer-path 'color-expression-lexer)]
      [else default])))
