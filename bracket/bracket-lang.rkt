#lang racket

(require "lang/parser.rkt")
(error-print-source-location #t)

(require (submod "bracket.rkt" symbolic-application)
         (submod "bracket.rkt" bracket)
         "unparse.rkt")

(provide (for-syntax #%module-begin)
         #%module-begin)
(provide (all-from-out racket)
         unparse)

(define-syntax (DeclareVars stx)
  (syntax-case stx ()
    [(_ sym ...)
     #'(begin 
         (define sym 'sym) ...
         (set! sym 'sym) ...)]))

(provide (all-defined-out))
