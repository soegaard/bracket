#lang racket
(provide show show-enabled configure)
 
(define show-enabled (make-parameter #f))
 
(define (show v)
  (when (show-enabled)
    (display v)))
 
(define (configure data)
  (show-enabled #t))

