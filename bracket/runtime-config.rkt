#lang racket
(provide configure)

;;;
;;; This file configures the repl in DrRacket/racket,
;;; when the bracket language is used.
;;;

(require bracket/lang/reader 
         bracket/unparse)
 
(define (configure data)
  (current-read-interaction read0)
  #;(define old-eval (current-eval))  
  #;(current-eval (λ (form) (displayln (list 'eval: form)) 
                    (define val (old-eval form))
                    (displayln (list 'eval-result: val))
                    val))
  (define old-print (current-print))
  #;(current-print (λ (val) (displayln (list 'print: val)) (old-print val)))
  (current-print 
   (λ (val) 
     (unless (void? val)
       (displayln (unparse val)))))
  )


(define (read0 src ip)
  ;(displayln (list 'read0 src ip))
  (define expr (read-expression-syntax src ip))
  (current-read-interaction read1)
  ;(displayln (list 'read0 expr))
  expr)
    
(define (read1 src ip)
  ;(displayln (list 'read1 src ip))
  (current-read-interaction read0)
  eof)
