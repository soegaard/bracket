#lang scheme
(provide #%infix)

(require scheme/stxparam)
(define-syntax-parameter #%infix
  (λ (stx)
    (syntax-case stx ()
      [(_ expr) #'expr])))