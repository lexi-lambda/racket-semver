#lang info

(define collection 'multi)

(define pkg-desc "implementation of semantic versioning in racket")
(define pkg-authors '(lexi.lambda))
(define version "0.0.0")

(define deps
  '("base"
    "typed-racket-lib"
    "alexis-util"))
(define build-deps
  '("racket-doc"
    "scribble-lib"
    "typed-racket-doc"
    "typed-racket-more"))
