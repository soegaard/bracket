#lang racket

;;; TODO: Implement variants of for/min

(provide for/max for*/max
         for/max+index for*/max+index)

(define-syntax (for/max stx)
  (syntax-case stx ()
    [(_ clauses . defs+exprs)
     (with-syntax ([original stx])
       #'(let-values
             ([(i m j)
               (for/fold/derived 
                original ([index #f] [max-so-far -inf.0] [i 0]) clauses
                (define x (let () . defs+exprs))
                (if (> x max-so-far)
                    (values i x (+ i 1))
                    (values index max-so-far (+ i 1))))])
           m))]))

(define-syntax (for/max+index stx)
  (syntax-case stx ()
    [(_ clauses . defs+exprs)
     (with-syntax ([original stx])
       #'(let-values
             ([(i m j)
               (for/fold/derived 
                original ([index #f] [max-so-far -inf.0] [i 0]) clauses
                (define x (let () . defs+exprs))
                (if (> x max-so-far)
                    (values i x (+ i 1))
                    (values index max-so-far (+ i 1))))])
           (values i m)))]))

(define-syntax (for*/max stx)
  (syntax-case stx ()
    [(_ clauses . defs+exprs)
     (with-syntax ([original stx])
       #'(let-values
             ([(i m j)
               (for/fold/derived 
                original ([index #f] [max-so-far -inf.0] [i 0]) clauses
                (define x (let () . defs+exprs))
                (if (> x max-so-far)
                    (values i x (+ i 1))
                    (values index max-so-far (+ i 1))))])
           m))]))

(define-syntax (for*/max+index stx)
  (syntax-case stx ()
    [(_ clauses . defs+exprs)
     (with-syntax ([original stx])
       #'(let-values
             ([(i m j)
               (for/fold/derived 
                original ([index #f] [max-so-far -inf.0] [i 0]) clauses
                (define x (let () . defs+exprs))
                (if (> x max-so-far)
                    (values i x (+ i 1))
                    (values index max-so-far (+ i 1))))])
           (values i m)))]))

