#lang scheme
(require scribble/eval
         (planet cce/scheme:4:1/planet))

(provide the-eval)

(define the-eval
  (let ([the-eval (make-base-eval)])
    (the-eval `(require (planet ,(this-package-version-symbol))))
    the-eval))