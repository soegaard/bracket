#lang racket

(require "../graphics/graphics.rkt")

(define-syntax (declare/provide-vars stx)
  (syntax-case stx ()
    [(_ id ...)
     #'(begin
         (define id 'id) ...
         (provide id) ...)]))

(provide Graphics)

(declare/provide-vars
 Blend Darker Hue Lighter
 Circle Disk Line Point Rectangle
 Text Thickness
 ; Colors
 Red Blue Green Black White Yellow
 ; Options
 ImageSize PlotRange)
