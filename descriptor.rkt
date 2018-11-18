#lang racket
(provide (struct-out record-descriptor))

;;; Record Descriptor

(struct record-descriptor
  (name   ; the name of the record
   fields ; list of identifiers
   types  ; list of: #f or descriptor
   )
  #:transparent)

; fields: list of identifiers (names of the fields)
; types:  for each field either #f or a descriptor which means the fields is expected
;         to hold a value of the described type



