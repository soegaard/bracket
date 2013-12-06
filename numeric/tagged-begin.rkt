#lang racket
(provide tagged-begin)

;;; INTRODUCTION

; This is a little macro that resembles the Common Lisp tagbody construct
;  <http://www-2.cs.cmu.edu/Groups/AI/html/hyperspec/HyperSpec/Body/speope_tagbody.html#tagbody>
; See also "Applications of Continuations" of Daniel P. Friedman.

;;; MOTIVATION

; Many algorithms is specified in an imperative manner
; in the literature [See Example 5 from Knuth]. For a no-brain-
; conversion to Scheme tagged-begin is convenient.

;;; SYNTAX

;  (tagged-begin
;     (<tag> | <expression>)* )  

;  where <tag> is a symbol and duplicate tags are not allowed.


;;; SEMANTICS

; The form evaluates the expressions in a lexical environment
; that provides functions go and return both of one argument to
; transfer control.

; The expressions in tagged-begin are evaluated sequentially.
; If no expressions are left (void) is returned.

; If an expression evaluates (go tag) then control is transfered
; to the expression following tag. The tags have lexical scope.
; The dynamic extent of tag is indefinite.  An (go tag) is allowed
; to tranfer control to an outer tagbody. The call (go tag) has the
; proper tail recursive property, even in situation where the call
; syntactically is not in tail position.

; If (return <expression>) is evaluted, the value of <expression> is
; the value of the entire tagged-begin form.


;;; EXAMPLES

;  See below the implementation.

;;; IMPLEMENTATION

; Tagged begin is here implemented as a syntax-case macro.
; The rewrite rule is taken from Daniel P. Friedmans
; "Applications of Continuations".


;  (tagged-begin
;    tag_1 e1 ...          ; If the body doesn't begin with a tag
;    ...                   ; the macro inserts a fresh one
;    tag_n-1 e_n-1 ...
;    tag_n   en ...)

; expands to

;  ((let/cc go
;     (let ([return (lambda (v) (go (lambda () v)))])
;       (letrec
;           ([tag_1   (lambda () e1    ... (tag2))]
;            ...
;            [tag_n-1 (lambda () e_n-1 ... (tag_n))]
;            [tag_n   (lambda () e_n   ... (return (void)))]
;            (tag_1))))))

; where (let/cc k expr ...) is short for (call/cc (lambda (k) expr ...)))]))


(require 
 (for-syntax 
  racket
  (only-in (lib "list.ss" "srfi" "1") 
           drop-right take-while)))

(define-syntax (tagged-begin stx)
  (define tag? identifier?)
  (define (non-tag? o) (not (tag? o)))
  
  (define (generate-binding tag-exprs next-tag)
    (match tag-exprs
      [(list tag exprs)  
       (quasisyntax/loc stx [#,tag (lambda () #,@exprs (#,next-tag))])]))
  
  (define (generate-last-binding tag-exprs return)
    (match tag-exprs
      [(list tag exprs)  
       (quasisyntax/loc stx [#,tag (lambda () #,@exprs (#,return (void)))])]))
  
  (syntax-case stx ()
    [(tagged-begin . tag/exprs-stx)
     (let ([tes (syntax->list #'tag/exprs-stx)])
       ; introduce a dummy start-tag, if the tagged-begin starts with an expression
       (when (not (tag? (car tes)))
         (set! tes (cons #'start tes)))
       (let* ([first-tag       (car tes)]
              [tag-exprs-list  (let loop ([tes tes]
                                          [rev-result '()])
                                 (if (null? tes)
                                     (reverse rev-result)
                                     (let ([p tes])
                                       (if (tag? (car p))
                                           (loop (cdr tes)
                                                 (cons (list (car p) (take-while non-tag? (cdr p)))
                                                       rev-result))
                                           (loop (cdr tes)
                                                 rev-result)
                                           ))))
                               #;(list-ec (:pairs p tes)
                                          (if (tag? (car p)))
                                          (list (car p) (take-while non-tag? (cdr p))))
                               ]
              [tags            (map car tag-exprs-list)])
         ; tag-exprs-list = ( (tag_1 (e1 ...))   (tag_2 (e2 ...)) ... )
         (with-syntax ([go     (syntax-local-introduce (syntax/loc stx go))]
                       [return (syntax-local-introduce (syntax/loc stx return))])
           #`((let/cc go
                (let ([return (lambda (v) (go (lambda () v)))])
                  (letrec
                      (#,@(map generate-binding 
                               (drop-right tag-exprs-list 1)
                               (cdr tags))
                       #,(generate-last-binding (last tag-exprs-list) #'return))
                    (#,first-tag))))))))]))

(module* test #f
  
  ; Example 1 (tagged-begin returns (void))
  (displayln
   (let ([i 0])
     (tagged-begin
      loop (set! i (+ i 1))
      (when (< i 41) (go loop)))
     i))
  
  ; Example 2 (tagged-begin returns 42)
  (displayln
   (let ([i 0])
     (tagged-begin
      loop (set! i (+ i 1))
      (when (< i 42) (go loop))
      (return i))))
  
  ; Example 3 (tagged-begin returns 43)
  (displayln
   (let ([i 0])
     (tagged-begin
      loop (set! i (+ i 1))
      (go b)
      a    (when (< i 43) (go loop))
      (return i)
      b    (go a))))
  
  ; Example 4 ( <http://www.emacswiki.org/cgi-bin/wiki.pl?StateMachine> )
  
  (let ((a 0))
    (tagged-begin
     start
     (set! a 0)
     part-1
     (set! a (+ a 1))
     (displayln a)
     (cond
       ((>= a  9)  (go end))
       ((even? a)  (go part-1))
       (else       (go part-2)))
     part-2
     (set! a (+ a 1))
     (go part-1)
     end
     (displayln "We're done printing the odd numbers between 0 and 10")))
  
  ; Example 5 ( Knuth: "The Art of Computer Programming", vol1, p.176)
  
  ; Inplace inversion of a permutation represented as a vector.
  
  (define permutation (vector 'dummy 6 2 1 5 4 3))      ; (Knuth counts from 1 not 0 :-) )
  (define n           (- (vector-length permutation) 1))
  (define (X i)       (vector-ref permutation i))
  (define (X! i j)    (vector-set! permutation i j))
  
  (let ([m 0] [i 0] [j 0])
    (tagged-begin 
     I1   ; Initialize
     (set! m n)
     (set! j -1)
     I2   ; Next element
     (set! i (X m))
     (when (< i 0) (go I5))
     I3   ; Invert one
     (X! m j)
     (set! j (- m))
     (set! m i)
     (set! i (X m))
     I4   ; End of cycle?
     (when (> i 0) (go I3))
     (set! i j)
     I5   ; Store final value
     (X! m (- i))
     I6   ; Loop on m
     (set! m (- m 1))
     (when (> m 0) (go I2))))
  
  (displayln permutation)
  
  ; Example 6 (The CommonLisp Hyper Spec examples of tagbody)     
  
  (define val 'foo)
  (tagged-begin
   (set! val 1)
   (go a)
   c     (set! val (+ val 4))
   (go b)
   (set! val (+ val 32))
   a     (set! val (+ val 2))
   (go c)
   (set! val (+ val 64))
   b     (set! val (+ val 8)))
  (displayln val)
  
  (define (f1 flag)
    (let ((n 1))
      (tagged-begin 
       (set! n (f2 flag (lambda () (go out))))
       out
       (display n))))
  
  (define (f2 flag escape)
    (if flag (escape) 2))
  
  (displayln (f1 #f))
  (displayln (f1 #t))
  
  ; Example 7
  ;   Demonstrates lexical scoping of tagged-begins,
  ;   and that an inner tagged-begin can use an outer tag.
  
  (tagged-begin
   a (tagged-begin
      (go b))
   b (return 'hello-world))
  
  ; Demonstrates that tags are lexically shadowed.
  (tagged-begin
   a (tagged-begin
      (go b)
      (return 'wrong)
      b (go c))
   b (return 'wrong)
   c (return 'correct))
  
  )