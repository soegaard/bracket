(module main "bracket-lang.rkt"
  (require (submod (file "/Users/soegaard/Dropbox/GitHub/bracket/bracket/bracket.rkt") bracket)
           (submod (file "/Users/soegaard/Dropbox/GitHub/bracket/bracket/bracket.rkt") symbolic-application))
  (define-syntax (#%infix stx) (syntax-case stx () ((_ expr) #'expr)))
  (define expt Power)
  (define + Plus)
  (define - Minus)
  (define * Times)
  (define / Quotient)
  (define = Equal)
  (define sqrt Sqrt)
  (define list List)
  (define list-ref List-ref)
  (define-syntax (define stx) (syntax-case stx () ((_ . more) #'(Define . more))))
  (#%infix (begin (begin (begin 
                           (begin
                             (begin
                               (begin
                                 (begin
                                   (begin
                                     (begin
                                       (begin
                                         (begin
                                           (begin
                                             (begin
                                               (begin
                                                 (begin (begin (begin (begin (begin (begin (begin (begin (begin (begin (begin (begin (begin (begin (begin (DeclareVars x y z x0 y0 z0 w) (* (list 2 3) (list 4 5))) (* 2 (list 3 4))) (* (list 3 4) 5)) (define (Fold f b xs) (foldl f b (rest xs)))) (define (sum-list xs) (Fold Plus 0 xs))) (define (RRange i j) (if (>= i j) (list) (Cons i (Range (+ i 1) j))))) (define (sum f x i j) (sum-list (Map (lambda (k) (Substitute f (= x k))) (Range i j))))) (sum (expt x 2) x 1 4)) (define (dot v w) (sum-list (* v w)))) (define (norm v) (sqrt (dot v v)))) (define (proj v w) (* (/ (dot v w) (expt (norm w) 2)) w))) (define (line point normal) (dot (- point (list x y)) normal))) (define a (list x y))) (define b (list z w))) (define bug-in-next-two-lines 2)) (* a b)) (dot a b)) (norm a)) (norm b)) (proj a b)) (line (list x0 y0) (list z w))) (define x 0)) (= x 0)) (If (= x 0) 1 2)) (If (= x 0) 1 2)) (If (= x 42) 1 2)) (If (+ 3 z) 1 2)) 
                         (define x 3)) x))
  )
