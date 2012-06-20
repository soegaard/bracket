#lang at-exp scheme
;; This is not in the use for the moment.
(provide $ $quote $quote-syntax #%infix)

(require "parameter.ss"
         scheme/port
         scheme/stxparam
         (for-syntax scheme)
         ;(planet soegaard/infix/parser)
         ;(for-syntax (planet soegaard/infix/parser))
         "parser.rkt"
         (for-syntax "parser.rkt"))

(define-syntax ($quote stx)
  (syntax-case stx ()
    [(_ item ...)
     (with-syntax ([(q ...) (local-expand #'($ item ...) 'expression #f)])
       #''(#%infix (q ...)))]))

(define-syntax ($quote-syntax stx)
  (syntax-case stx ()
    [(_ item ...)
     (with-syntax ([(q ...) (local-expand #'($ item ...) 'expression #f)])
       #'#'(#%infix (q ...)))]))
  
(define-syntax ($ stx)
  (syntax-case stx ()
    [(_ item ...)
     (let* ([from-at? (syntax-property stx 'scribble)])
       (if from-at?
           ; reintroduce the original (discarded) indentation
           (with-syntax 
               ([(item ...) 
                 (let loop ([items (syntax->list #'(item ...))])
                   (if (null? items)
                       '()
                       (let* ([fst  (car items)]
                              [prop (syntax-property fst 'scribble)]
                              [rst  (loop (cdr items))])
                         (cond [(eq? prop 'indentation) rst]
                               [(not (and (pair? prop)
                                          (eq? (car prop) 'newline)))
                                (cons fst rst)]
                               [else (cons (datum->syntax fst (cadr prop) fst)
                                           rst)]))))])
             #'($$ item ...))
           #'($$ item ...)))]))

(define-syntax ($$ stx)
  (syntax-case stx ()
    [(_ str str* ...)
     (let* ([from-at? (syntax-property stx 'scribble)]
            [offset   (if from-at? 0 1)]
            [ip (open-input-string 
                 (apply string-append
                        (map syntax->datum 
                             (syntax->list #'(str str* ...)))))])
       ;(display "from-at?: ") (display from-at?) (newline)
       ;(display "str: ") (display #'str) (newline)
       ;(display "str*: ") (display #'(str* ...)) (newline)
       ;(display "stx: ") (display stx) (newline)
       (port-count-lines! ip)
       (let* ([line (syntax-line #'str)]
              [col  (+ (syntax-column #'str) offset)]
              [pos  (+ (syntax-position #'str) offset -1)])
         ;(display (list line col pos)) (newline)
         (let ([result
                (parse-expression 
                 (if from-at?
                     (datum->syntax 
                      #'str
                      (apply string-append
                             (map syntax->datum 
                                  (syntax->list #'(str str* ...))))
                      (list (syntax-source #'str)
                            line col pos
                            (syntax-span #'str)))
                     #'str)
                 ip)])
           ;(display "result: ") (display result) (newline)
           result)))]))

