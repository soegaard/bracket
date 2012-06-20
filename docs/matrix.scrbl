#lang scribble/manual
@(require ; "../math-scribble/math-scribble.rkt"
          (for-label "../matrix.rkt"
                     racket
                     racket/contract
                     racket/dict
                     racket/base)
          "../matrix.rkt"
          racket/string
          racket/list
          scribble/eval
          "pr-math.rkt")

@setup-math


@;@$$[(tex-matrix (matrix 2 3 1 2 3 4 5 6))]
@;@$$[(build-tex-matrix 2 3 (λ (i j) (format "a_{~a~a}" i j)))]

@(define (make-matrix-eval print-matrix-as-snip)
   (let ([eval (make-base-eval)])
     (eval `(begin
              (require "../matrix.rkt"
                       slideshow/pict
                       "pr-math.rkt")              
              (begin
                (require racket/list)
                (define (subscript a i [j #f])
                  (if j
                      (hb-append (scale (text a) 2)
                                 (scale (text (format "~a~a" i j)) 0.75))
                      (hb-append (scale (text a) 2)
                                 (scale (text (format "~a" i)) 0.75))))
                (define (a i j)
                  (subscript "a" i j))
                (define (b i j)
                  (subscript "a" i j))
                (define (add-brackets p)
                  (define bw 5)
                  (define bh 1)
                  (let ([h (pict-height p)])
                    (hc-append 
                     ; left bracket
                     (filled-rectangle 1 h)
                     (vl-append (filled-rectangle bw bh) 
                                (blank bw (- h (* 2 bh)))
                                (filled-rectangle bw bh))
                     ; contents
                     p 
                     ; right bracket
                     (vl-append (filled-rectangle bw bh) 
                                (blank bw (- h (* 2 bh)))
                                (filled-rectangle bw bh))
                     (filled-rectangle 1 h))))
                (define (build-array-pict m n f)
                  (apply hc-append
                         (add-between
                          (for/list ([j (in-range n)])
                            (apply vr-append
                                  
                                   (for/list ([i (in-range m)])
                                     (let ([f_ij (f i j)])
                                       (cond
                                         [(number? f_ij)
                                          (text (number->string f_ij))]
                                         [(pict? f_ij) f_ij]
                                         [(string? f_ij)
                                          (text f_ij)]
                                         [else (error)])))))
                          (blank 5))))
                (define (build-matrix-pict m n f)
                  (add-brackets
                   (build-array-pict m n f)))
                (define (matrix->pict M)
                  (define-values (m n) (matrix-size M))
                  (add-brackets
                   (build-array-pict 
                    m n (λ (i j) (matrix-ref M i j)))))
                ,(if print-matrix-as-snip
                     '(current-print
                       (let ([print (current-print)])
                         (define (maybe->matrix v)
                           (if (matrix? v) (matrix->pict v) v))
                         (λ (v) (print 
                                 (cond 
                                   [(matrix? v) (matrix->pict v)]
                                   [(list? v)   (map maybe->matrix v)]
                                   [else v])))))
                     '(begin)) 
                )))
     eval))

@;@racketinput[(matrix 2 3 1 2 3 4 5 6)]
@;@$[(tex-matrix (matrix 2 3 1 2 3 4 5 6))]

@racketblock[> (matrix 2 3 1 2 3 4 5 6)
             #,(math-in (tex-matrix (matrix 2 3 1 2 3 4 5 6)))]
         


@(define matrix-eval (make-matrix-eval #f))
@(define matrix-eval/snip (make-matrix-eval #t))

@;defmodulelang[@racketmodname[matrix] #:module-paths ("../matrix.rkt")]
@defmodule[(file "../matrix.rkt")]

@(define the-eval (make-base-eval))
@(the-eval '(require "../matrix.rkt"))

@title[#:tag "matrix"]{Matrix Library}

A @italic{matrix} is a rectangular array of numbers. A typical example:
@centered{
@interaction-eval-show[#:eval matrix-eval 
                              (scale
                               (matrix->pict
                                (matrix 3 2  1 2 3 4 5 6)) 2)]}
The horizontal lines are called @italic{rows} and the vertical lines
are called @italic{columns}. The matrix above has @italic{m}=3 rows 
and @italic{n}=2 columns and is an example of an @italic{3x2}-matrix.
In general a matrix with @italic{m} rows and @italic{n} columns is 
called an @italic{mxn}-matrix.

Matrices with one row (1xn) only are called @italic{row vectors} and
matrices with one column (mx1) only are called @italic{column vectors}.

The @italic{entries} or @italic{elements} of a matrix is referred to by their 
row and column number. In this library the row and columns are counted from 0.
@centered{
@interaction-eval-show[#:eval matrix-eval 
                              (build-matrix-pict 3 2 (λ (i j) (a i j)))]}
Note: In standard mathematical notation row and column numbers are counted from 1.

@;-----------------------------------------------------------------------

@section{A basic example}


First require the libary:
@racketinput[(require "matrix.rkt")]
Create a 2x3 matrix with entries 1, 2, 3, 4, 5, and, 6.
@interaction[#:eval matrix-eval
                    (matrix 2 3  1 2 3 4 5 6)]
Display the matrix as a picture:
@interaction[#:eval matrix-eval
                    (matrix->pict 
                     (matrix 2 3  1 2 3 4 5 6))]
Let's change the print handler, so we don't need to call @racket{matrix->pict}
ourselves.
@interaction[#:eval matrix-eval
                    (current-print
                     (let ([print (current-print)])
                       (λ (v) (print (if (matrix? v) (matrix->pict v) v)))))
                    (matrix 2 2  1 2 3 4)]
The basic operations are addition, subtraction and multiplication.
@interaction[#:eval matrix-eval/snip
                    (define A (matrix 2 2  1 2 3 4))
                    (define B (matrix 2 2  1 -2 3 -4))
                    A
                    B
                    (matrix-add A B)
                    (matrix-sub A B)
                    (matrix-mul A B)]
Scale a matrix by a factor 2.
@interaction[#:eval matrix-eval/snip
                    (matrix-scale 2 (matrix 2 2  1 2 3 4))]
Multiply a matrix on a column-vector.
@interaction[#:eval matrix-eval/snip
                    (matrix-mul (matrix 2 2  1 2 3 4)
                                (column-vector 1 0))]

@;---------------------------------------------------------------------

@section[#:tag "matrix constructors"]{Constructors}

@defproc[(matrix [m index/c] [n index/c] [x number?] ...)
         matrix
         ]{
  Construct a mxn-matrix with elements @racket[x ...].
  @interaction[#:eval matrix-eval/snip
                      (matrix 2 2  1 2 3 4)]}

@defproc[(row-vector [x number?] ...)
         matrix]{
  Construct a row vector (a 1xn-matrix) with elements @racket[x ...].
  @interaction[#:eval matrix-eval/snip
                      (row-vector 1 2 3)]}

@defproc[(column-vector [x number?] ...)
         matrix]{
  Construct a column vector (a mx1-matrix) with elements @racket[x ...].
  @interaction[#:eval matrix-eval/snip
                      (column-vector 1 2 3)]}

@defproc[(make-matrix [m size/c] [n size/c] [x number?])
         matrix]{
  Construct a mxn-matrix where all entries are @racket[x].
  @interaction[#:eval matrix-eval/snip
                      (make-matrix 2 3  4)]
  }

@defproc[(build-matrix [m size/c] [n size/c] [f (index/c index/c -> number?)])
         matrix]{
  Construct a mxn-matrix where element (i,j) is @racket[(f i j)].
  @interaction[#:eval matrix-eval/snip
                      (build-matrix 3 4 +)]
  }

@defproc[(submatrix [M matrix?] [i index/c] [j index/c] [m size/c] [n size/c])
         matrix]{
  Construct a mxn-matrix with elements from row i to i+m and from column j to j+m.
  @interaction[#:eval matrix-eval/snip
                      (define A (build-matrix 5 5 (λ (i j) (+ (* i 5) j))))
                      A
                      (submatrix A 2 3 1 2)]
  }

@defproc[(matrix-augment [M matrix?] [N matrix?])
         matrix]{
  Augment the matrices M and N by "appending" their columns.
  The number of rows in M and N must be the same.
  @interaction[#:eval matrix-eval/snip
                      (matrix-augment (matrix 2 2  1 2 
                                                   3 4)
                                      (matrix 2 3  5 6  7  
                                                   8 9 10))]
  }

@defproc[(matrix-augment* [Ms (listof matrix?)])
         matrix]{
  Augment the matrices in the list Ms "appending" their columns.
  The number of rows in alle the matrices must be the same.
  @interaction[#:eval matrix-eval/snip
                      (matrix-augment* (list (matrix 2 1  1 2)
                                             (matrix 2 1  3 4)
                                             (matrix 2 1  5 6)))]
  }

@defproc[(matrix-copy [M matrix?])
         matrix]{
  Copy the matrix M. 
  @interaction[#:eval matrix-eval/snip
                      (matrix-copy (matrix 1 2  3 4))]
  }

@defproc[(matrix-row [M matrix?] [i index/c])
         matrix]{
  Construct a row vector from the i'th column of M. 
  @interaction[#:eval matrix-eval/snip
                      (define M (matrix 2 4  1 2 3 4 5 6 7 8))
                      M
                      (matrix-row M 1)]
  }

@defproc[(matrix-column [M matrix?] [j index/c])
         matrix]{
  Construct a column vector from the j'th column of M. 
  @interaction[#:eval matrix-eval/snip
                      (define M (matrix 2 4  1 2 3 4 5 6 7 8))
                      M
                      (matrix-column M 2)]
  }

@defproc*[([(matrix-identity [m size/c]) matrix]
           [(matrix-identity [m size/c] [n size/c]) matrix])]{
  Return m x n matrix with ones on the diagonal and zeros elsewhere.
  If only one argument is given, a square matrix is produced.
  @interaction[#:eval matrix-eval/snip
                      (matrix-identity 3)
                      (matrix-identity 3 2)]
  }
                                                             
@defproc[(matrix-diagonal [xs (listof number?)])
         matrix]{
  Construct a square matrix with elements from xs on the diagonal and 0 elsewhere.
  @interaction[#:eval matrix-eval/snip
                      (matrix-diagonal '(1 2 3))]
  }
                

@section[#:tag "matrix operations"]{Operations}

@defproc[(matrix-scale [s number?] [M matrix?])
         matrix]{
  Multiply each element of M with s.
  @interaction[#:eval matrix-eval/snip
                      (matrix-scale 3 (matrix 2 2  1 2 3 4))]
  }

@defproc[(matrix-add [M matrix?] [N matrix?])
         matrix]{
  Return the sum of M and N. 
  @interaction[#:eval matrix-eval/snip
                      (define M (matrix 2 2  1 2 3 4))
                      (define N (matrix 2 2  1 -2 3 -4))
                      (list M N (matrix-add M N))]
  }

@defproc[(matrix-mul [M matrix?] [N matrix?])
         matrix]{
  Return the product of M and N. 
  The number of columns in M must be equal to the number of rows in N.
  @interaction[#:eval matrix-eval/snip
                      (define M (matrix 2 2  1 2 3 4))
                      (define N (matrix 2 2  2 0 0 1))
                      (list M N (matrix-mul M N))]
  }

@defproc[(matrix-linear [a number?] [M matrix?] [b number?] [N matrix?])
         matrix]{
  Return the linear combination of M and N @math-in{a*M + b*N}. 
  @interaction[#:eval matrix-eval/snip
                      (define M (matrix 2 2  1 1 1 0))
                      (define N (matrix 2 2  0 0 1 1))
                      (list M N (matrix-linear -1 M 2 N))]
  }


@section[#:tag "matrix unary operators"]{Unary Operators}
                
@section[#:tag "matrix comprehensions"]{Comprehensions}
@section{Binary Operators}
                  
                  