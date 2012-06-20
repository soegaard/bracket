#lang racket

(require "lang/parser.rkt")
(error-print-source-location #t)

(require (submod "bracket.rkt" symbolic-application)
         (submod "bracket.rkt" bracket))

(provide (all-from-out racket))
(provide (all-defined-out)
         (for-syntax #%module-begin)
         #%module-begin)

(define-syntax (DeclareVars stx)
  (syntax-case stx ()
    [(_ sym ...)
     #'(begin (define sym 'sym) ...)]))


