#lang racket
;;; Instructions:

;;  1. Use ctrl+m to insert a math box
;;  2. Write an infix expression in the math box. Evaluate with enter. [Use shift+enter to insert line breaks]
;;  3. E.g. write plot[sin] to draw a graph of sine.

;;  Examples:
;;    x_min[-pi]; x_max[pi];
;;    y_min[-pi]; y_max[pi];
;;    map[ (λk.plot[(λx.cos[x+k*pi/4])]),
;;         {-4,-3,-2,-1,0,1,2,3,4}]; "done"

;;
;; TODO
;;
;; Editor:      Bold and italic used more than once should toggle.
;; Editor:      Text size.
;; Editor:      Save and load.
;; Editor:      When inserting part copied from MathBox, automatically insert new MathBox.
;; Parser:      Extend f(x):=expr syntax to 2 or more variables.
;; Evaluation:  Get syntax-location from math-text%
;; Keymaps:     Insertion of greek, and, mathematical characters.
;; Maxima:      Maxima backend. See send-lisp-command and receive-list in maxima.rkt.
;; Yacas:       ?
;; LaTeX:      
;; PlotCanvas:  Add status icons in upper, left corner. Add zooms.
;; Plot:        Error message for the case plot[not-a-function] (partially done - 0 is plotted)
;; MathBox:     When to remove old evaluation result?
;; NSolve:      Root polishing algorithms. See GSL.
;; Snips:       Right-click menu: Options such output on/off.
;; Snips:       Represent equations, integrals etc with small graphs ala Wolfram Alpha.
;; Educational: Recognize standard equations and offer step by step solutions.

;; LIMBO

;; PlotCanvas: Add "copy plot as image". set-clipboard-image is unimplmented :-(
;;             (Fixed on OS X)


;; DONE
;;
;; MathBox:    Evaluating an expression with an error, removes previous result.
;; MathBox:    Enter evaluates. Shift+enter inserts newline.
;; Parser:     plot[function[sin]]  Handle parsing of ]] better
;; Parser:     Declare variable: The operator := now expands to define
;; Parser:     f(x):=expr now defines one variable functions
;; Parser:     Handle - in names. _ is converted to -
;; Editor:     Keybinding for bold and italic work.
;; Editor:     Keybindings for λ α β γ δ ε ∆ Λ.
;; Editor:     Gets focus right after start.
;; Plot:       Parameters x-min, x-max, y-min, y-max for plot window.
;; Plot:       Function/macro plot[...] introduced.
;; Plot:       Added plot-horizontal-line and plot-vertical-line.
;; Plot:       Remove old plots when reevaluating math boxes.
;; Plot:       Different colors are used, when the user doesn't specify a color.
;; NSolve:     Implemented bisection and Brent.
;; Snips:      Right-click menu: Evaluation options such output on/off. 
;; Errors:     Errors and warnings can be signaled with user-error and user-warning


;; IDEAS

;; Behaviour when dragging parabolas and lines.
;; http://www.geogebra.org/forum/viewtopic.php?f=22&t=27113
;; Write API for MathType.
;; Support copy-paste with MathML ?

(require racket/gui framework
         (except-in plot plot points)
         (prefix-in plot: plot)
         "infix/main.rkt"
         "infix/parser.rkt"
         (planet williams/science/math)
         "root-finding.rkt"
         "clipboard.rkt")

;;; 
;;; NUMERICAL
;;;

(define (nsolve f lower upper)
  (define max-iterations 150)
  (define eps-rel double-epsilon)
  (define eps-abs double-epsilon)
  (define safe-f (λ (x) (if (inexact? x) (f x) (f (->inexact x)))))
  
  
  (with-handlers ([exn:fail?   (λ (e) (user-error (exn-message e)))]
                  [(λ (x) #t)  (λ (e) (user-error "internal error: nsolve"))])
    (find-root brent-solver safe-f (->inexact lower) (->inexact upper) eps-abs eps-rel 150)))

(define (->inexact x)
  (if (exact? x) (exact->inexact x) x))

;;;
;;;
;;;

(define all-math-texts '())
(define all-renderers '())    

(define-syntax (add-renderer! stx)
  (syntax-case stx ()
    [(_ expr) #'(set! all-renderers (append all-renderers (list expr)))]))


;;;
;;; PLOTTING
;;; 

(define x-min   (make-parameter -10.0))
(define x-max   (make-parameter +10.0))
(define y-min   (make-parameter -10.0))
(define y-max   (make-parameter +10.0))
(define x-label (make-parameter #f))
(define y-label (make-parameter #f))

(define plot-color (make-parameter 0))
(define (next-plot-color) (plot-color (add1 (plot-color))))

(define (plot-function 
         f [x-min #f] [x-max #f] [y-min #f] [y-max #f]
         [color (begin (next-plot-color) (plot-color))] 
         [width (line-width)]
         [style (line-style)] [alpha (line-alpha)]
         [label #f] [samples (line-samples)])
  (define (ensure-number-function f)
    (lambda (x)
      (let ([v (f x)])
        (if (number? v) v 0))))  
  (add-renderer! (function (ensure-number-function f) x-min x-max
    #:y-min y-min     #:y-max y-max
    #:samples samples #:color color
    #:width width     #:style style 	 
    #:alpha alpha     #:label label)))

#;(function f	 	 	 	 
 	 [	x-min	 	 	 	 
 	 	x-max	 	 	 	 
 	 	#:y-min y-min	 	 	 	 
 	 	#:y-max y-max	 	 	 	 
 	 	#:samples samples	 	 	 	 
 	 	#:color color	 	 	 	 
 	 	#:width width	 	 	 	 
 	 	#:style style	 	 	 	 
 	 	#:alpha alpha	 	 	 	 
 	 	#:label label])

#;(lines	 	vs	 	 	 	 
                        [	#:x-min x-min	 	 	 	 
 	 	#:x-max x-max	 	 	 	 
 	 	#:y-min y-min	 	 	 	 
 	 	#:y-max y-max	 	 	 	 
 	 	#:color color	 	 	 	 
 	 	#:width width	 	 	 	 
 	 	#:style style	 	 	 	 
 	 	#:alpha alpha	 	 	 	 
 	 	#:label label])	

; given points in ps, draws points and connect them with line segments
(define (plot-lines 
         ps [x-min #f] [x-max #f] [y-min #f] [y-max #f]
         [color (line-color)] [width (line-width)]
         [style (line-style)] [alpha (line-alpha)]
         [label #f])
  (let ([vs (map (λ (p) (vector (first p) (second p))) ps)])
    (add-renderer! (lines vs  
                          #:x-min x-min #:x-max x-max
                          #:y-min y-min #:y-max y-max	 	 	 	 
                          #:color color	 	 	 	 
                          #:width width #:style style	 	 	 	 
                          #:alpha alpha #:label label))))

(define (plot-vertical-line x0 
         [color (line-color)] [width (line-width)]
         [style (line-style)] [alpha (line-alpha)]
         [label #f])
  (plot-lines (list (list x0 (y-min)) (list x0 (y-max)))
              (x-min) (x-max) (y-min) (y-max)
              color width style alpha label))

(define (plot-horizontal-line y0 
         [color (line-color)] [width (line-width)]
         [style (line-style)] [alpha (line-alpha)]
         [label #f])
  (plot-lines (list (list (x-min) y0) (list (x-max) y0))
              (x-min) (x-max) (y-min) (y-max)
              color width style alpha label))

(define (plot-equation f1 f2 solutions
                       [x-min #f] [x-max #f] [y-min #f] [y-max #f]
                       [color1 (line-color)] [color2 (add1 (line-color))] [width (line-width)]
                       [style (line-style)] [alpha (line-alpha)]
                       [label #f] [samples (line-samples)])
  (plot-function f1 x-min x-max y-min y-max color1 width style alpha label samples)
  (plot-function f2 x-min x-max y-min y-max color2 width style alpha label samples)
  (for ([x0 (in-list solutions)])
    (let ([y0 (f1 x0)])
      (plot-arrow (list x0 y0) (list x0 0) #:color "black"))))

(define (plot-arrow p1 p2 #:color [color (line-color)])
  ; Note: Arrow size depends on distance from p1 p2
  ; Introduce fixed size?
  (match-define (list x1 y1) p1)
  (match-define (list x2 y2) p2)
  (define dx (- x2 x1))
  (define dy (- y2 y1))
  (define angle (if (and (zero? dy) (zero? dx)) 0 (atan dy dx)))
  (define dist (sqrt (+ (sqr dx) (sqr dy))))
  (define head-r (* 2/5 dist))
  (define head-angle (* 1/6 pi))
  (define dx1 (* (cos (+ angle head-angle)) head-r))
  (define dy1 (* (sin (+ angle head-angle)) head-r))
  (define dx2 (* (cos (- angle head-angle)) head-r))
  (define dy2 (* (sin (- angle head-angle)) head-r))
  (parameterize ([line-color color])
    (plot-lines (list (list x1 y1) (list x2 y2)))
    (plot-lines (list (list x2 y2) (list (- x2 dx1) (- y2 dy1))))
    (plot-lines (list (list x2 y2) (list (- x2 dx2) (- y2 dy2))))))

(define-syntax (plot stx)
  (syntax-case stx ()
    [(_ expr)
     #'(plot-function expr) ]
    [(_ expr id)
     #'(plot-function (λ (id) expr))]
    [(_ expr id from to)
     #'(plot-function (λ (id) expr) from to)]
    ; TODO: signal error
    ))


(define (plot-point-label x y [label #f] [color "black"] [size 10])
  (add-renderer! (point-label (vector x y) label #:color color #:size size)))

(define (plot-function-label f x [label #f] [color "black"] [size 10])
  (add-renderer! (function-label f x label #:color color #:size size)))



#;(function-label 
   f x	 
   [label	 
    #:color color	 
    #:size size	 
    #:family family	 
    #:anchor anchor	 
    #:angle angle	 
    #:point-color point-color	 
    #:point-fill-color point-fill-color	 
    #:point-size point-size	 
    #:point-line-width point-line-width	 
    #:point-sym point-sym	 
    #:alpha alpha])


(define (points ps)
  (define (to-vector p)
    (cond [(and (list? p) (= (length p) 2))
           (apply vector p)]
          [else (error 'points "list of length 2 expected, got ~a" p)]))
  (set! all-renderers (cons (plot:points (map to-vector ps)) all-renderers))
  "done")


(define math-text%
  (class* text% ()
    (inherit get-text 
             get-start-position
             select-all
             delete
             insert
             set-position
             change-style)
    (super-new)
    (set! all-math-texts (append all-math-texts (list this)))
    
    ; Activation/Deactivation of evaluation of the math box
    (define is-active? #t)  ; #t will
    (define/public (active?) is-active?)
    (define/public (activate) (set! is-active? #t))
    (define/public (deactivate) (set! is-active? #f))
    
    (define/override (copy-self)
      (let ([ed (new math-text%)])
        (send ed insert (get-text))
        ed))
    
    (define/public (evaluate)
      (let* ([expr (remove-old-evaluation-result (get-text))]
             [val  (if is-active? (evaluate-expr expr) #f)]
             [pos  (get-start-position)])
        (if val
            (begin
              ; normal evaluation
              (select-all)
              (delete)
              (insert expr)                
              (change-style red-delta)                
              (insert " => ")
              (insert val)
              (when pos
                (set-position pos))
              (send plot-canvas refresh))
            (begin
              ; error in expression
              (select-all)
              (delete)
              (insert expr)))))))

(define (new-math-snip)
  (let* ([editor (new math-text%)]
         [math-snip (new math-editor-snip% [editor editor])]
         [keymap (send editor get-keymap)])
    (send editor set-max-undo-history 1024)
    ; (send math-snip set-style (send text-editor get-style))
    (send editor change-style (let ([∆ (new style-delta%)]) (send ∆ set-delta 'change-size 20) ∆))
    (install-math-snip-keymap math-snip)
    
    math-snip))

(define math-editor-snip%
  (class* (editor-snip:decorated-mixin editor-snip%) ()
    (inherit get-editor border-visible?
             get-margin get-inset 
             get-min-width get-max-width 
             get-min-height get-max-height)
    (super-new)
    
    (define/override (copy)
      (let* ([math-snip (new math-editor-snip%)]
             [editor (send (get-editor) copy-self)])
        (send math-snip set-editor editor)
        (send editor set-max-undo-history 1024)
        (install-math-snip-keymap math-snip)
        math-snip))
    
    (define math-box-menu (new popup-menu% [title "Popupmenu"]))
    (define mi (new menu-item% [label "Deactivate"] [parent math-box-menu]
                      [callback  (λ (r e)
                                   (let ([ed (send this get-editor)])
                                     (if (send ed active?)
                                         (begin
                                           (send ed deactivate)
                                           (send ed evaluate) ; In order to remove old evaluation result
                                           (send mi set-label "Activate"))
                                         (begin
                                           (send ed activate)
                                           (send mi set-label "Deactivate")))))]))
    
    (define/override (on-event dc x y editorx editory ev)
      (if (eq? (send ev get-event-type) 'right-up)
          (send (send this get-admin) popup-menu math-box-menu this 
                (- (send ev get-x) editorx) (- (send ev get-y) editory))
          (super on-event dc x y editorx editory ev)))
    
    (define/override (get-corner-bitmap)
      (let ([bitmap (make-object bitmap% 15 15)])
        (send (new bitmap-dc% [bitmap bitmap])  draw-text "$" 0 0)
        bitmap))
    (define/override (get-position)
      'left-top)
    ))


(define my-text%
  (class* text% ()
    (inherit)
    (super-new)))

(define show-x-axis #t)
(define show-y-axis #t)
(define show-grid #f)

(define-syntax (toggle! stx)
  (syntax-case stx () [(_ id) #'(set! id (not id))]))

(define plot-canvas-menu (new popup-menu% [title "Popupmenu"]))
(new menu-item% [label "Delete all plots"] [parent plot-canvas-menu]
     [callback (λ (r e) (set! all-renderers '()) (send plot-canvas refresh))])
(define show-hide-menu (new menu% [label "Show/hide"] [parent plot-canvas-menu]))
(new menu-item%	[label "x-axis"] [parent show-hide-menu] 
     [callback  (λ (r e) (toggle! show-x-axis) (send plot-canvas refresh))])
(new menu-item%	[label "y-axis"] [parent show-hide-menu] 
     [callback  (λ (r e) (toggle! show-y-axis) (send plot-canvas refresh))])
(new menu-item%	[label "grid"] [parent show-hide-menu] 
     [callback  (λ (r e) (toggle! show-grid) (send plot-canvas refresh))])
(new menu-item% [label "Copy as image"] [parent plot-canvas-menu]
     [callback 
      (λ (r e) 
        (set-clipboard-bitmap
         (let* ([bm (send plot-canvas make-bitmap (send plot-canvas get-width) (send plot-canvas get-height))]
                [dc (new bitmap-dc% [bitmap bm])])
           (draw-plot dc)
           bm))
                
        ; TODO: Use this when set-clipboard-bitmap is implemented in the racket gui
        #;(send the-clipboard set-clipboard-bitmap
                (let* ([bm (send plot-canvas make-bitmap (send plot-canvas get-width) (send plot-canvas get-height))]
                       [dc (new bitmap-dc% [bitmap bm])])
                  (draw-plot dc)
                  bm)))])


(define plot-canvas%
  (class* canvas% ()
    (super-new)
    (define/override (on-event ev)
      (if (eq? (send ev get-event-type) 'right-up)
          (send plot-canvas popup-menu plot-canvas-menu 
                (send ev get-x)
                (send ev get-y))
          (super on-event ev)))))

;;;
;;;
;;;


(define frame               (new frame% [label "Racket CAS"] [width 400] [height 400]))
(define vertical-panel      (new vertical-panel% [parent frame] [stretchable-height #f]))
(define evaluate-all-button (new button% [parent vertical-panel] [label "Evaluate All"]
                                 [callback 
                                  (λ (r e) 
                                    (set! all-renderers '())
                                    (send plot-canvas refresh)
                                    (for-each (λ (m) (send m evaluate)) all-math-texts)
                                    (send plot-canvas refresh))]))

(define horizontal-panel    (new panel:horizontal-dragable% [parent frame]))

(define canvas              (new editor-canvas% [parent horizontal-panel] [min-width 200] [min-height 200] [vert-margin 10] [horiz-margin 10]))
(define text-editor         (new my-text%  ))
(send text-editor set-max-undo-history 1024)
(send canvas set-editor text-editor)
(send text-editor set-styles-sticky #t)
(send text-editor change-style (let ([∆ (new style-delta%)]) (send ∆ set-delta 'change-size 20) ∆))
(send text-editor set-caret-owner #f 'global)

(define (list-if p . more)
  (if p more '()))

(define (draw-plot dc)
  (let ()
    (send dc suspend-flush)
    (plot/dc (append (list-if show-x-axis (x-axis) )
                     (list-if show-y-axis (y-axis) )
                     (list-if show-grid  (x-tick-lines) (y-tick-lines))
                     all-renderers)
             dc
             0 0
             (send plot-canvas get-width)
             (send plot-canvas get-height)
             #:x-min   (x-min)
             #:x-max   (x-max)
             #:x-label (x-label)
             #:y-min   (y-min)
             #:y-max   (x-max)
             #:y-label (y-label)
             #:title #f)
    (send dc resume-flush)))

(define plot-canvas  
  (new plot-canvas% [parent horizontal-panel]
       [min-width 200] [min-height 200] [vert-margin 10] [horiz-margin 10]
       [paint-callback (λ (c dc) (draw-plot dc))]))


(define status-panel    (new horizontal-panel% [parent frame] [stretchable-height #f] [alignment '(left center)]))
(define status-message  (new message% [parent status-panel] [label "Status:"] [auto-resize #t]))

(define menu-bar  (new menu-bar% [parent frame]))
(define menu-edit (new menu% [label "Edit"] [parent menu-bar]))
(define menu-font (new menu% [label "Font"] [parent menu-bar]))
(append-editor-operation-menu-items menu-edit #f)
(append-editor-font-menu-items menu-font)


(application:current-app-name "Racket CAS") ; Appears in help menu



(define (make-color-delta color)
  (send (make-object style-delta%) set-delta-foreground color))

(define red-delta  (make-color-delta (make-object color% "red")))
(define blue-delta (make-color-delta (make-object color% "blue")))

(define (remove-old-evaluation-result str)
  (let ([m (regexp-match #rx"(.*) =>.*" str)])
    (if m (second m) str)))

(define (evaluate-expr str)
  (with-handlers 
      ([(λ (e) #t) 
        (λ (e) (begin
                 (display e) (newline)
                 (send status-message set-label
                       (format "eval: invalid expression, got ~a" str))
                 #f))])
    (format "~a" 
            (eval-syntax 
             (parse-expression 
              (datum->syntax #'here str 
                             (list 'src-name 1 0 1 (string-length str)))
              (open-input-string str))))))

(define (install-math-snip-keymap math-snip)
  (let ([keymap (send (send math-snip get-editor) get-keymap)])
    
    (define (register name char shortcuts)
      (let ([insert-name (string-append "insert-" name)])
        (send keymap add-function insert-name
              (λ (ed e) (send ed insert char)))
        (for ([shortcut shortcuts])
          (send keymap map-function shortcut insert-name))))
    
    (send keymap add-function "evaluate-math" 
          (λ (ed e) (send ed evaluate)))
    (send keymap add-function "left-willing-to-leave"
          (λ (ed e)
            (let ([pos (send ed get-start-position)])
              (if (= pos 0)
                  (begin
                    (send text-editor set-caret-owner #f)
                    (send text-editor set-position 
                          (send text-editor get-snip-position math-snip)))
                  (send ed move-position 'left)))))
    (send keymap add-function "right-willing-to-leave"
          (λ (ed e)
            (let ([pos (send ed get-start-position)])
              (if (= pos (send ed last-position))
                  (begin
                    (send text-editor set-caret-owner #f)
                    (send text-editor set-position 
                          (+ 1 (send text-editor get-snip-position math-snip))))
                  (send ed move-position 'right)))))
    (send keymap add-function "newline"
      (λ (ed e) (send ed insert #\newline)))
    
    (register "lambda"  #\λ '("d:\\" "c:\\"))
    (register "Lambda"  #\Λ '("c:L"))
    (register "alpha"   #\α '("c:a"))
    (register "beta"    #\β '("c:b"))
    (register "gamma"   #\γ '("c:g"))
    (register "delta"   #\δ '("c:d"))
    (register "epsilon" #\ε '("c:e"))
    (register "rho"     #\ρ '("c:r"))
    (register "Gamma"   #\Γ '("c:G"))
    (register "Delta"   #\∆ '("c:D"))
    
    (send keymap map-function "s:enter" "newline")
    (send keymap map-function "enter" "evaluate-math")
    (send keymap map-function "~s:left" "left-willing-to-leave")
    (send keymap map-function "~s:right" "right-willing-to-leave")))



;;;
;;; KEYBINDINGS FOR THE DOCUMENT EDITOR
;;;

(define keymap (send text-editor get-keymap))

;(add-editor-keymap-functions keymap) 
;(add-text-keymap-functions keymap)

(send keymap add-function "insert-math" 
      (λ (in e)
        ; get the current selection, if any,
        (let ([start (box #f)] [end (box #f)])
          (send in get-position start end)
          (let ([selected-text
                 (if (and (unbox start) (unbox end))
                     (send in get-text (unbox start) (unbox end))
                     #f)]
                ; make a new math-snip, and insert the selected text
                [math-snip (new-math-snip)])
            (send text-editor insert math-snip)
            (send text-editor set-caret-owner math-snip 'display)  
            (when selected-text
              (send (send math-snip get-editor) insert selected-text))))))

(send keymap add-function "left-willing-to-enter-math-snip" 
      (λ (in e)
        ; left:   if the position before the caret is a math-editor, enter it
        (let* ([pos (send in get-start-position)]
               [snip-pos (box #f)]
               [snip (send in find-snip pos 'before-or-none snip-pos)])
          (if (and snip (is-a? snip math-editor-snip%))
              (begin
                (send in set-caret-owner snip)
                (let ([ed (send snip get-editor)])
                  (send ed set-position (send ed last-position))))
              (send in move-position 'left)))))

(send keymap add-function "right-willing-to-enter-math-snip" 
      (λ (in e)
        ; right:  if the position after the caret is a math-editor, enter it
        (let* ([pos (send in get-start-position)]
               [snip-pos (box #f)]
               [snip (send in find-snip pos 'after-or-none snip-pos)])
          (if (and snip 
                   (is-a? snip editor-snip%)
                   (is-a? (send snip get-editor) math-text%))
              (begin
                (send in set-caret-owner snip)
                (let ([ed (send snip get-editor)])
                  (send ed set-position 0)))
              (send in move-position 'right)))))
(send keymap add-function "bold"
      (λ (in e)
        ; TODO: twice => toggle 
        (send text-editor change-style 
              (make-object style-delta% 'change-weight 'bold))))
(send keymap add-function "italic"
      (λ (in e)
        ; TODO: twice => toggle 
        (send text-editor change-style 
              (make-object style-delta% 'change-style 'italic))))


(send keymap map-function "c:m" "insert-math")
(send keymap map-function "d:m" "insert-math") ; OS X cmd
(send keymap map-function "~s:left" "left-willing-to-enter-math-snip")
(send keymap map-function "~s:right" "right-willing-to-enter-math-snip")
(send keymap map-function "c:b" "bold")   ; Win   ctrl
(send keymap map-function "d:b" "bold")   ; OS X  cmd
(send keymap map-function "c:i" "italic") ; Win   ctrl
(send keymap map-function "d:i" "italic") ; OS X  cmd

;;;
;;; ERRORS AND WARNINGS
;;;

(define (user-error text)
  (let ([msg (format "ERROR: ~a" text)])
    (send status-message set-label msg)
    msg))

(define (user-warning text)
  (let ([msg (format "WARNING: ~a" text)])
    (send status-message set-label msg)
    msg))

;;;
;;; BIG BANG
;;; 

; (send frame create-status-line)
; (send frame set-status-text "Ready")
(send frame maximize #t)
(send frame show #t)

; (graphical-read-eval-print-loop)

