#lang typed/racket/base

(require racket/function
         (for-syntax racket/base
                     racket/syntax
                     syntax/parse))

(provide generate-comparisons)

(begin-for-syntax
  (define-syntax-rule (format-identifier format id)
    (format-id id #:source id format id)))

; Automagically generates comparison functions from a function that returns 0, 1 or -1 to
; compare two values! Supply it with the output prefix and a comparator function, and it will
; do the rest. You can also supply a #:type to type the input paramters for Typed Racket, and
; you can supply an #:adapter to convert input values into values to be handed to the comparator.
(define-syntax (generate-comparisons stx)
  (syntax-parse stx
    [(_ out-name:id comparison-fn:expr
        (~optional (~seq #:type type:id) #:defaults ([type #'Any]))
        (~optional (~seq #:adapter adapter-fn:expr) #:defaults ([adapter-fn #'identity])))
     (define/with-syntax out-name=? (format-identifier "~a=?" #'out-name))
     (define/with-syntax out-name>? (format-identifier "~a>?" #'out-name))
     (define/with-syntax out-name<? (format-identifier "~a<?" #'out-name))
     (define/with-syntax out-name>=? (format-identifier "~a>=?" #'out-name))
     (define/with-syntax out-name<=? (format-identifier "~a<=?" #'out-name))
     #'(begin
         (define compare comparison-fn)
         (define adapt adapter-fn)
         (define (out-name=? [a : type] [b : type])
           (= (compare (adapt a) (adapt b)) 0))
         (define (out-name>? [a : type] [b : type])
           (= (compare (adapt a) (adapt b)) 1))
         (define (out-name<? [a : type] [b : type])
           (= (compare (adapt a) (adapt b)) -1))
         (define (out-name>=? [a : type] [b : type])
           (let ([v (compare (adapt a) (adapt b))])
             (or (= v 0) (= v 1))))
         (define (out-name<=? [a : type] [b : type])
           (let ([v (compare (adapt a) (adapt b))])
             (or (= v 0) (= v -1))))
         )]))
