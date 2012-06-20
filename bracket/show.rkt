#lang racket
 
(provide show show-enabled)
 
(define show-enabled (make-parameter #f))
 
(define (show v)
  (when (show-enabled)
    (display v)))
