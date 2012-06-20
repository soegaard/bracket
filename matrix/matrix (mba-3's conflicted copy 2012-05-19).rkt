#lang racket

(struct matrix (m n d) ; d is a vector of rows. 
  #:transparent)

(define (make-matrix m n v)
  ; Make m x n - matrix. Elements are all v.
  (matrix m n (make-vector (* m n) v)))

(define (matrix-size M)
  (values (matrix-m M) (matrix-n M)))

(define (matrix-ref M i j)
  (let ([d (matrix-d M)]
        [n (matrix-n M)])
    (vector-ref d (+ (* i n) j))))

(define (matrix-set! M i j x)
  (let ([d (matrix-d M)]
        [n (matrix-n M)])
    (vector-set! d (+ (* i n) j) x)))

(define-syntax (for/matrix stx)
  (syntax-case stx ()
    ((for/matrix m-expr n-expr (clause ...) . defs+exprs)
     (syntax/loc stx
       (let ([m m-expr] [n n-expr])
         (matrix m n (for/vector #:length (* m n) (clause ...) . defs+exprs)))))))

(define-syntax (for*/matrix stx)
  (syntax-case stx ()
    ((for/matrix m-expr n-expr (clause ...) . defs+exprs)
     (syntax/loc stx
       (let ([m m-expr] [n n-expr])
         (matrix m n (for*/vector #:length (* m n) (clause ...) . defs+exprs)))))))

(define-syntax (for/matrix-sum stx)
  (syntax-case stx ()
    ((for/matrix (clause ...) . defs+exprs)
     (syntax/loc stx
       (let ([ms (for/list (clause ...) . defs+exprs)])
         (foldl matrix-add (first ms) (rest ms)))))))

(define-syntax (for*/matrix-sum stx)
  (syntax-case stx ()
    ((for/matrix (clause ...) . defs+exprs)
     (syntax/loc stx
       (let ([ms (for/list (clause ...) . defs+exprs)])
         (foldl matrix-sum (first ms) (rest ms)))))))

(define (in-matrix M)
  (in-vector (matrix-d M)))

(define-sequence-syntax in-row
  (λ () #'in-row/proc)
  (λ (stx)
    (syntax-case stx ()
      [[(x) (_ M-expr r-expr)]
       #'((x)
          (:do-in
           ([(M r n d) 
             (let ([M1 M-expr])
               (values M1 r-expr (matrix-n M1) (matrix-d M1)))])
           (begin 
             (unless (matrix? M) 
               (raise-type-error 'in-row "expected matrix, got ~a" M))
             (unless (integer? r) 
               (raise-type-error 'in-row "expected row number, got ~a" r)))
           ([j 0])
           (< j n)
           ([(x) (vector-ref d (+ (* r n) j))])
           #true
           #true
           [(+ j 1)]))]
      [[(i x) (_ M-expr r-expr)]
       #'((i x)
          (:do-in
           ([(M r n d) 
             (let ([M1 M-expr])
               (values M1 r-expr (matrix-n M1) (matrix-d M1)))])
           (begin 
             (unless (matrix? M) 
               (raise-type-error 'in-row "expected matrix, got ~a" M))
             (unless (integer? r) 
               (raise-type-error 'in-row "expected row number, got ~a" r)))
           ([j 0])
           (< j n)
           ([(x) (vector-ref d (+ (* r n) j))]
            [(i) j])
           #true
           #true
           [(+ j 1)]))])))

(define-sequence-syntax in-col
  (λ () #'in-col/proc)
  (λ (stx)
    (syntax-case stx ()
      [[(x) (_ M-expr s-expr)]
       #'((x)
          (:do-in
           ([(M s n m d) 
             (let ([M1 M-expr])
               (values M1 s-expr (matrix-n M1) (matrix-m M1) (matrix-d M1)))])
           (begin 
             (unless (matrix? M) 
               (raise-type-error 'in-row "expected matrix, got ~a" M))
             (unless (integer? s) 
               (raise-type-error 'in-row "expected col number, got ~a" s)))
           ([j 0])
           (< j m)
           ([(x) (vector-ref d (+ (* j n) s))])
           #true
           #true
           [(+ j 1)]))]
      [[(i x) (_ M-expr s-expr)]
       #'((x)
          (:do-in
           ([(M s n m d) 
             (let ([M1 M-expr])
               (values M1 s-expr (matrix-n M1) (matrix-m M1) (matrix-d M1)))])
           (begin 
             (unless (matrix? M) 
               (raise-type-error 'in-row "expected matrix, got ~a" M))
             (unless (integer? s) 
               (raise-type-error 'in-row "expected col number, got ~a" s)))
           ([j 0])
           (< j m)
           ([(x) (vector-ref d (+ (* j n) s))]
            [(i) j])
           #true
           #true
           [(+ j 1)]))])))



(define (matrix: m n . xs)
  (unless (= (length xs) (* m n))
    (error 'matrix: "Matrix size and number of elements do not match."))
  (matrix m n (list->vector xs)))

(define (matrix-identity m [n m])
  ; m x n matrix with ones on the diagolnal and zeros elsewhere
  (for*/matrix m n 
    ([i (in-range n)]
     [j (in-range n)])
    (if (= i j) 1 0)))

(define (matrix-transpose M)
  (define-values (m n) (matrix-size M))
  (for*/matrix n m ([x (in-matrix M)]) x))

#;(define (matrix-inverse M)
    (define-values (m n) (matrix-size M))
    (unless (= m n) (error 'matrix-inverse "expected square matrix, got ~a" M))
    ...)

(define (matrix-same-size? M N)
  (define-values (m n) (matrix-size M))
  (define-values (s t) (matrix-size N))
  (and (= m s) (= n t)))

(define (matrix-add M N)
  (define-values (m n) (matrix-size M))
  (for/matrix m n ([x (in-matrix M)] [y (in-matrix N)])
    (+ x y)))

(define (matrix-sub M N)
  (define-values (m n) (matrix-size M))
  (for/matrix m n ([x (in-matrix M)] [y (in-matrix N)])
    (- x y)))

(define (matrix-scale s M)
  (define-values (m n) (matrix-size M))
  (for/matrix m n ([x (in-matrix M)])
    (* s x)))

(define (matrix-linear a M b N)
  (unless (matrix-same-size? M N) (error))
  (define-values (m n) (matrix-size M))
  (for/matrix m n ([x (in-matrix M)]
                   [y (in-matrix N)])
    (+ (* a x) (* b y))))

(define (matrix-mul M N)
  (define-values (m n) (matrix-size M))
  (define-values (s t) (matrix-size N))
  (unless (= n s) (error))
  (for*/matrix m t ([i (in-range m)]
                    [j (in-range t)])
    (for/sum ([x (in-row M i)]
              [y (in-col N j)])
      (* x y))))

(define (matrix-augment M N)
  (define-values (m n) (matrix-size M))
  (define-values (s t) (matrix-size N))
  (unless (= m s) (error))
  (for*/matrix m (+ n t) ([i (in-range m)]
                          [j (in-range (+ n t))])
    (if (< j n)
        (matrix-ref M i j)
        (matrix-ref N i (- j n)))))

(define (matrix-copy M)
  (define-values (m n) (matrix-size M))
  (matrix m n (vector-copy (matrix-d M))))

(define (matrix-swap-rows! M r s)
  (define-values (m n) (matrix-size M))
  (for ([(j t) (in-row M r)])
    (matrix-set! M r j (matrix-ref M s j))
    (matrix-set! M s j t)))

(define (matrix-scale-row! M r a)
  (define-values (m n) (matrix-size M))
  (for ([(j x) (in-row M r)])
    (matrix-set! M r j (* a x))))

(define (matrix-add-scaled-row! M r a s)
  (define-values (m n) (matrix-size M))
  (for ([(j x) (in-row M r)]
        [y     (in-row M s)])
    (matrix-set! M r j (+ x (* a y)))))

(define (matrix-gauss-eliminate! M [unitize-pivot-row? #f])
  (define-values (m n) (matrix-size M))
  (for ([i (in-range m)])
    ; find non-zero element
    (let ([p (for/first ([j (in-range i m)])
               (and (not (zero? (matrix-ref M j i))) j))])
      (when p
        ; swap if neccessary
        (unless (= i p)
          (matrix-swap-rows! M i p))
        (let ([x_ii (matrix-ref M i i)])
          ; maybe unitize pivot
          (when unitize-pivot-row?
            (matrix-scale-row! M i (/ x_ii))))
        (let ([x_ii (matrix-ref M i i)])
          ; remove elements below pivot
          (for ([j (in-range (+ i 1) m)])
            (let ([x_ji (matrix-ref M j i)])
              (unless (zero? x_ji)
                (matrix-add-scaled-row! M j (- (/ x_ji x_ii)) i)))))))))

(define (matrix-gauss-jordan-eliminate! M [unitize-pivot-row? #f])
  (define-values (m n) (matrix-size M))
  (for ([i (in-range m)])
    ; find non-zero element
    (let ([p (for/first ([j (in-range i m)])
               (and (not (zero? (matrix-ref M j i))) j))])
      (when p
        ; swap if neccessary
        (unless (= i p)
          (matrix-swap-rows! M i p))
        (let ([x_ii (matrix-ref M i i)])
          ; maybe unitize pivot
          (when unitize-pivot-row?
            (matrix-scale-row! M i (/ x_ii))))
        (let ([x_ii (matrix-ref M i i)])
          ; remove elements below pivot        
          (for ([j (in-range (+ i 1) m)])
            (let ([x_ji (matrix-ref M j i)])
              (unless (zero? x_ji)
                (matrix-add-scaled-row! M j (- (/ x_ji x_ii)) i))))
          ; remove elements above pivot
          (for ([j (in-range i)])
            (let ([x_ji (matrix-ref M j i)])
              (unless (zero? x_ji)
                (matrix-add-scaled-row! M j (- (/ x_ji x_ii)) i)))))))))

(define (submatrix M i j m n)
  (define-values (s t) (matrix-size M))
  (for*/matrix m n ([k (in-range i (+ i m))]
                    [l (in-range j (+ j n))])
    (matrix-ref M k l)))

(define (matrix-inverse M)
  (define-values (m n) (matrix-size M))
  (unless (= m n) (error 'matrix-inverse "matrix not square"))
  (let ([MI (matrix-augment M (matrix-identity m))])
    (matrix-gauss-jordan-eliminate! MI #t)
    (submatrix MI 0 m m m)))

(define (matrix-trace M)
  (define-values (m n) (matrix-size M))
  (unless (= m n) (error 'matrix-inverse "matrix not square"))
  (for/sum ([i m]) (matrix-ref M i i)))

(define (matrix-solve M b)
  (define-values (m n) (matrix-size M))
  (define-values (s t) (matrix-size b))
  (unless (= t 1) (error 'matrix-solve "expected column vector (i.e. r x 1 - matrix), got: ~a " b))
  (unless (= m s) (error 'matrix-solve "expected column vector with same number of rows as the matrix"))
  (let ([Mb (matrix-augment M b)])
    (matrix-gauss-jordan-eliminate! Mb #t)
    (submatrix Mb 0 m m 1)))

(define (matrix-determinant M)
  ; TODO: Specialize for small sizes
  (define-values (m n) (matrix-size M))
  (let ([M (matrix-copy M)])
    (matrix-gauss-eliminate! M)
    (for/product ([i (in-range m)])
      (matrix-ref M i i))))

(define (row-vector-product v w)
  (for/sum ([x (in-row v 0)]
            [y (in-row w 0)])
    (* x y)))

(define (column-vector-product v w)
  (for/sum ([x (in-col v 0)]
            [y (in-col w 0)])
    (* x y)))

(define (matrix-norm-row M r)
  (sqrt (for/sum ([x (in-row M r)]) (* x x))))

(define (matrix-norm-col M s)
  (sqrt (for/sum ([x (in-col M s)]) (* x x))))

(define (vector-norm v)
  (sqrt
   (cond 
     [(column-vector? v)
      (column-vector-product v v)]
     [(row-vector? v)
      (row-vector-product v v)]
     [else (error 'vector-norm)])))

(define (vector-normalize w) 
  (matrix-scale (/ (vector-norm w)) w))

(define (column-vector? v)
  (= 1 (matrix-n v)))

(define (row-vector? v)
  (= 1 (matrix-m v)))

(define (projection v w)
  (cond
    [(andmap column-vector? (list v w))
     (matrix-scale (/ (column-vector-product v w)
                      (column-vector-product w w))
                   w)]
    [(andmap row-vector? (list v w))
     (matrix-scale (/ (row-vector-product v w)
                      (row-vector-product w w))
                   w)]
    [else 
     (error 'project "expected either two row-vectors or two column-vectors, got: ~a and ~a" v w)]))


(define (row-vector . xs)
  (matrix 1 (length xs) (list->vector xs)))

(define (column-vector . xs)
  (matrix (length xs) 1 (list->vector xs)))

(define (matrix-column M j)
  (define-values (m n) (matrix-size M))
  (for/matrix m 1 ([x (in-col M j)]) x))

(define (matrix-row M j)
  (define-values (m n) (matrix-size M))
  (for/matrix 1 n ([x (in-row M j)]) x))

(define (projection-on-orthogonal-basis v bs)
  (if (matrix? bs)
      (for/matrix-sum ([j (matrix-n bs)])
        (let ([b (matrix-column bs j)])
          (projection v b)))
      (for/matrix-sum ([b bs])
        (projection v b))))

(define (projection-on-orthonormal-basis v bs)
  (if (matrix? bs)
      (for/matrix-sum ([j (matrix-n bs)])
        (let ([b (matrix-column bs j)])
          (matrix-scale (column-vector-product v b) b)))
      (for/matrix-sum ([b bs])
        (matrix-scale (column-vector-product v b) b))))

(define (gram-schmidt-orthogonal ws)
  ; ws is a basis, return orthogonal basis
  (cond 
    [(empty? ws) '()]
    [(empty? (rest ws)) ws]
    [else 
     (reverse 
      (for/fold ([vs (list (first ws))]) ([w (rest ws)])
        (cons (matrix-sub w (projection-on-orthogonal-basis w vs))
              vs)))]))

(define (gram-schmidt-orthonormal ws)
  ; ws is a basis, return orthogonal basis
  (map vector-normalize
       (gram-schmidt-orthogonal ws)))

(define (matrix-augment* ms)
  ; TODO: improve efficieny here
  (let ([ms (reverse ms)])
    (foldl matrix-augment (first ms) (rest ms))))

(define (matrix->column-vectors M)
  (define-values (m n) (matrix-size M))
  (for/list ([j n])
    (matrix-column M j)))

(define (matrix->row-vectors M)
  (define-values (m n) (matrix-size M))
  (for/list ([i m])
    (matrix-row M i)))

(define (matrix-QR M)
  ; compute the QR-facorization
  ; 1) QR = M 
  ; 2) columns of Q is are orthonormal
  ; 3) R is upper-triangular
  (define-values (m n) (matrix-size M))
  (let* ([Q (matrix-augment*
             (gram-schmidt-orthonormal 
              (matrix->column-vectors M)))]
         [R (for*/matrix n n 
              ([i n] [j n])
              (if (> i j) 
                  0
                  (for/sum ([x (in-col Q i)]
                            [y (in-col M j)])
                    (* x y))))])
    (values Q R)))

(define (improve Q+R)
  (let ([Q (first Q+R)]
        [R (second Q+R)])
    (let-values ([(Q1 R1) (matrix-QR (matrix-mul R Q))])
      (list Q1 R1))))

(define-syntax (def stx)
  (syntax-case stx ()
    [(_ . more) (syntax/loc stx (define . more))]))

(define-syntax (defv stx)
  (syntax-case stx ()
    [(_ . more) (syntax/loc stx (define-values . more))]))
                 

(define (step M)
  ; page 18 of http://www.mathworks.com/moler/eigs.pdf  
  (defv (m n) (matrix-size M))
  (def  s     (matrix-ref M (- n 1) (- n 1)))
  (def  sI    (matrix-scale s (matrix-identity n)))
  (defv (Q R) (matrix-QR (matrix-sub M sI)))
  (matrix-add (matrix-mul R Q) sI))

    



(require rackunit)
(check-equal? (make-matrix 2 1 0)   (matrix 2 1 #(0 0)))
(check-equal? (make-matrix 1 2 0)   (matrix 1 2 #(0 0)))
(check-equal? (matrix: 2 2 1 2 3 4) (matrix 2 2 #(1 2 3 4)))
(check-equal? (matrix-identity 2)   (matrix 2 2 #(1 0 0 1)))
(let ([M (matrix: 2 2 1 2 3 4)]) 
  (check-equal? (for*/list ([i 2] [j 2]) (matrix-ref M i j)) '(1 2 3 4)))
(let ([M (matrix: 2 2 1 2 3 4)])
  (matrix-set! M 1 1 5)
  (check-equal? (for*/list ([i 2] [j 2]) (matrix-ref M i j)) '(1 2 3 5)))
(check-equal? (matrix-add (matrix: 2 2 1 2 3 4) (matrix: 2 2 5 6 7 8))
              (matrix: 2 2 6 8 10 12))
(check-equal? (matrix-sub (matrix: 2 2 5 6 7 8) (matrix: 2 2 1 2 3 4))
              (matrix: 2 2 4 4 4 4))
(check-equal? (matrix-scale 2 (matrix: 2 2 1 2 3 4))
              (matrix: 2 2 2 4 6 8))
(check-equal? (matrix-mul (matrix: 2 2 1 2 3 4) (matrix: 2 2 5 6 7 8))
              (matrix: 2 2 19 22 43 50))
(check-equal? (matrix-augment (matrix: 2 2 1 2 3 4) (matrix: 2 2 5 6 7 8))
              (matrix: 2 4 1 2 5 6 3 4 7 8))
(check-equal? (matrix: 2 2 1 2 3 4) (matrix-copy (matrix: 2 2 1 2 3 4)))
(let ([M (matrix: 3 2 1 2 3 4 5 6)])
  (matrix-swap-rows! M 0 2)
  (check-equal? M (matrix: 3 2 5 6 3 4 1 2)))
(let ([M (matrix: 3 2 1 2 3 4 5 6)])
  (matrix-scale-row! M 1 2)
  (check-equal? M (matrix: 3 2 1 2 6 8 5 6)))
(let ([M (matrix: 3 2 1 2 3 4 5 6)])
  (matrix-add-scaled-row! M 1 2 0)
  (check-equal? M (matrix: 3 2 1 2 5 8 5 6)))
(check-equal? (submatrix (matrix: 3 3 1 2 3 4 5 6 7 8 9) 0 0 2 2)
              (matrix: 2 2 1 2 4 5))
(let ([M (matrix: 2 2 1 4 2 3)])
  (check-equal? (matrix-mul M (matrix-inverse M))
                (matrix-identity 2)))
(check-equal? (matrix-trace (matrix: 2 2 1 2 3 4)) 5)
(let* ([M (matrix: 2 2 1 5 2 3)]
       [b (matrix: 2 1 5 5)])
  (check-equal? (matrix-mul M (matrix-solve M b)) b))
(check-equal? (row-vector-product (matrix: 1 3 1 2 3) (matrix: 1 3 4 5 6))
              (+ (* 1 4) (* 2 5) (* 3 6)))
(check-equal? (column-vector-product (matrix: 3 1 1 2 3) (matrix: 3 1 4 5 6))
              (+ (* 1 4) (* 2 5) (* 3 6)))
(check-equal? (matrix-norm-row (matrix: 2 2 1 2 3 4) 0) (sqrt (+ 1 4)))
(check-equal? (matrix-norm-row (matrix: 2 2 1 2 3 4) 1) (sqrt (+ 9 16)))
(check-equal? (matrix-norm-col (matrix: 2 2 1 2 3 4) 0) (sqrt (+ 1 9)))
(check-equal? (matrix-norm-col (matrix: 2 2 1 2 3 4) 1) (sqrt (+ 4 16)))
(check-equal? (projection (row-vector 1 2 3) (row-vector 2 4 3)) 
              (matrix-scale 19/29 (row-vector 2 4 3))) 
(check-equal? (projection (column-vector 1 2 3) (column-vector 2 4 3))
              (matrix-scale 19/29 (column-vector 2 4 3)))
(check-equal? (matrix-column (matrix: 2 2 1 2 3 4) 0) (matrix: 2 1 1 3))
(check-equal? (matrix-column (matrix: 2 2 1 2 3 4) 1) (matrix: 2 1 2 4))
(check-equal? (matrix-row (matrix: 2 2 1 2 3 4) 0) (matrix: 1 2 1 2))
(check-equal? (matrix-row (matrix: 2 2 1 2 3 4) 1) (matrix: 1 2 3 4))
(check-equal? (projection-on-orthogonal-basis 
               (column-vector 3 -2 2) (list (column-vector -1 0 2) (column-vector 2 5 1)))
              (column-vector -1/3 -1/3 1/3))
(check-equal? (projection-on-orthogonal-basis 
               (column-vector 3 -2 2) (matrix: 3 2 -1 2 0 5 2 1))
              (column-vector -1/3 -1/3 1/3))
(check-equal? (projection-on-orthonormal-basis 
               (column-vector 1 2 3 4) 
               (list (matrix-scale 1/2 (column-vector  1  1  1 1))
                     (matrix-scale 1/2 (column-vector -1  1 -1 1))
                     (matrix-scale 1/2 (column-vector  1 -1 -1 1))))
              (column-vector 2 3 2 3))
(check-equal? (projection-on-orthonormal-basis 
               (column-vector 1 2 3 4)
               (matrix-scale 1/2 (matrix: 4 3 1 -1 1 1 1 -1 1 -1 -1 1 1 1)))
              (column-vector 2 3 2 3))
(check-equal? (gram-schmidt-orthonormal 
               (list (column-vector 1 0 1) (column-vector 1 1 1)))
              (list (matrix 3 1 '#(0.7071067811865475 0 0.7071067811865475)) 
                    (matrix 3 1 '#(0 1 0))))
(let-values ([(Q R) (matrix-QR (matrix: 3 2 1 1 0 1 1 1))])
  (check-equal? (list Q R)
                (list (matrix 3 2 '#(0.7071067811865475 0 0 1 0.7071067811865475 0))
                      (matrix 2 2 '#(1.414213562373095 1.414213562373095 0 1)))))


