#lang racket
(require pkg)

;;;
;;; Open this file in DrRacket and run it to install
;;; these packages:
;;;

; install : source -> void
(define (install source)
  (define (always) #t)
  (define (handle e) 
    (displayln (~a "Error occured while installing: " source))
    (displayln (exn-message e)))
  (with-handlers ([always handle]) 
    (apply pkg-install-command (list source))))

; memoize 
(install "memoize")






