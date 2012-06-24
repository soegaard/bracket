#lang racket
(provide show show-enabled configure)
 
(define show-enabled (make-parameter #f))
 
(define (show v)
  (when (show-enabled)
    (display v)))
 
(define (configure data)
  (show-enabled #t)
  (current-read-interaction read0))

(require bracket/lang/reader)

(define (without-lang-read src in)
  (parameterize ([read-accept-reader #f]
                 [read-accept-lang #f])
    (read-one-line src in)))

; XXX This is almost certainly wrong.
(define (read0 src ip)
  (displayln (list 'read0 src ip))
  (begin0
    (without-lang-read src ip)
    (current-read-interaction read1)))

(define (read1 src ip)
  (displayln (list 'read1 src ip))
  (current-read-interaction read0)
  eof)

(define (read2 src ip)
  (displayln (list 'read2 src ip))
  (current-read-interaction read0)
  eof)

;; at the repl, honu will only read a single line at a time regardless
;; of how many expressions it contains
(define (read-one-line name input)
  (define quit? #f)
  (define one-line
    (with-output-to-string
     (lambda ()
        (let loop ()
          (define next (read-char input))
          (when (eof-object? next)
            (set! quit? #t))
          (when (not (or (eof-object? next)
                         (char=? next #\newline)))
            (display next)
            (loop))))))
  (if quit?
    eof
    (read-syntax name (open-input-string one-line))))

