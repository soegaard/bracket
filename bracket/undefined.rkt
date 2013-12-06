#lang racket/base
(provide undefined undefined?)
(define undefined 'undefined)
(define (undefined? e) (eq? e 'undefined))
