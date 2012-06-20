#lang racket
(require "show.rkt")
 
(provide configure)
 
(define (configure data)
  (show-enabled #t))