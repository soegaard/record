#lang racket
(provide (struct-out record-descriptor))

;;; Record Descriptor

(struct record-descriptor
  (name   ; the name of the record
   fields ; list of identifiers
   )
  #:transparent)


