#lang scribble/manual
 
@title{Racket Math Libraries}
@author["Jens Axel Søgaard"]

These libraries is intended to become the backend for a CAS
written in Racket. A CAS requires libraries for a wide range
of mathematical concepts and algorithms. 

The matrix library implements matrices over Racket numbers.
The implementation is written in Racket, that is there are no
external dependencies.

The development version of this library is available at Github:
@(let ([url "https://github.com/soegaard"])
   (link url url))

@table-of-contents[]
@; ------------------------------------------------------------------------
@include-section["matrix.scrbl"]
@index-section[]
