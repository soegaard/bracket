#lang racket
 
(provide get-language-info)

(define (get-language-info data)
  (lambda (key default)
    (case key
      [(configure-runtime)
       '(#(bracket/runtime-config configure #f))]
      [else default])))
