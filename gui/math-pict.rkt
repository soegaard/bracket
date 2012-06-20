#lang racket
(require slideshow/pict
         slideshow)

(define (pict->snip p [scale-factor 5])
  (pict->bitmap 
   (scale p scale-factor)))

(define (left-bracket content-height line-width bracket-width)
  (define h content-height)
  (define bw bracket-width)
  (define lw line-width)  
  (hc-append 
   (filled-rectangle lw h)
   (vl-append (filled-rectangle bw lw)
              (blank bw (- h (* 2 lw)))
              (filled-rectangle bw lw))))

(define (right-bracket content-height line-width bracket-width)
  (define h content-height)
  (define bw bracket-width)
  (define lw line-width)  
  (hc-append 
   (vl-append (filled-rectangle bw lw)
              (blank bw (- h (* 2 lw)))
              (filled-rectangle bw lw))
   (filled-rectangle lw h)))

;;; Combiners

(define (over p q)
  (vc-append -4 q p))

(define (under p q)
  (vc-append p q))

(define (left p q)
  (hc-append q p))

(define (right p q)
  (hc-append p q))

(define (subscript p q)
  (hb-append p (scale q 5/12)))

(define (superscript p q)
  (ht-append p (scale q 5/12)))


(define (fraction p q)
  (define w (max (pict-width p) (pict-width q)))
  (vc-append p (hc-append (blank 1 0) (hline w 1) (blank 1 0)) q))

;;; Elements depending on one pict

(define (vector-arrow p)  
  (arrow (pict-width p) 0))

(define (bar p)
  (define w (pict-width p))
  (clip-descent
   (hc-append (blank 1 0) 
              (hline w 1)
              (blank 1 0))))

;;; Combination

(define (add f c p)
  ; (add bar over p) will put a bar over p
  (c p (f p)))

;;;


(define (add-brackets p)
  (define lw 1)
  (define bw 5)
  (define h (pict-height p))
  (hc-append
   (left-bracket h lw bw)
   p
   (right-bracket h lw bw)))


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

(require racket/gui/base)
(define (butt-hline w)
 (dc
  (λ (dc dx dy)
    (define pen (send dc get-pen))
    (send dc set-pen (send the-pen-list find-or-create-pen
                           (send pen get-color)
                           (send pen get-width)
                           'solid
                           'butt))
    (send dc draw-line dx dy (+ dx w) dy)
    (send dc set-pen pen))
  w 1))



(define (left-paren p)
  (define w (max 5 (ceiling (/ (pict-height p) 10))))
  (define h (pict-height p))
  (dc
   (λ (dc dx dy)
     (define pen (send dc get-pen))
     (send dc set-pen 
           (send the-pen-list find-or-create-pen
                 (send pen get-color)
                 (send pen get-width)
                 'solid
                 'butt))
     (send dc draw-arc (+ dx (/ w 2)) dy w h (/ pi 2) (* 3/2 pi))
     (send dc set-pen pen))
   w h))

(define (right-paren p)
  (define w (max 5 (ceiling (/ (pict-height p) 10))))
  (define h (pict-height p))
  (dc
   (λ (dc dx dy)
     (define pen (send dc get-pen))
     (send dc set-pen 
           (send the-pen-list find-or-create-pen
                 (send pen get-color)
                 (send pen get-width)
                 'solid
                 'butt))
     (send dc draw-arc (- dx (/ w 2)) dy w h (* 3/2 pi) (/ pi 2))
     (send dc set-pen pen))
   w h))

(define (add-paren p)
  (hc-append (left-paren p) p (right-paren p)))

;;; TESTS

; A 4x4 matrix

(scale
 (add-brackets
  (build-array-pict
   4 4 (λ (i j) (subscript (text "a") (text (format "~a~a" i j))))))
 5)

; bars
(scale (add bar over (text "a")) 5)

; fraction
(scale (fraction (text "x") (text "y")) 5)


