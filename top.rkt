#lang racket
(provide (for-syntax top))
(require (for-syntax syntax/parse))

; This version of top makes references to unbound identifiers evaluate
; to #f rather than raising an error.
(begin-for-syntax
  (define (top stx)
    (syntax-parse stx
      [(_top . id)
       (syntax/loc stx
         #f)])))

; Example:

; (let-syntax ([#%top top]) x)     ; evaluates to #f
; (let-syntax ([#%top top]) list)  ; evaluates to the list primitive
