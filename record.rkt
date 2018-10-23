#lang racket
;;;
;;; RECORDS
;;;

; Records are an extension of the standard Racket structure.
; A record (like a struct) has named fields each containing a value.

; Let's say a record of type r has a field named f.
; If v is an identifier bound to record of type r, then:

; (define-record r (f ...)) defines a new record type r with fields f, ...
; (declare v r)            declares that the variable v is bound to an instance of a record of type r
; v.f                      references the value corresponding to field f, it's equuivalent to (r-f v)
; (:= v.f e)               sets the field f to the result of evaluating e

; If a         variable v is bound to a record of type r which has a field f,
; and if the   variable v has been declared to be of type r,
; then         the expression  v.f  will evaluate to (r-f v).
; Here (r-f v) is the standard syntax used by structures.

(require (for-syntax syntax/parse racket/syntax)
         (for-syntax "descriptor.rkt" "top.rkt")
         (for-syntax (for-syntax racket/base)))
(require (for-syntax
             (for-syntax syntax/parse racket/base racket/syntax)))

;;; Helpers used to construct indentifiers.

(begin-for-syntax
  (begin 
    (define (format-id-record-descriptor id) (format-id id    "~a-record-descriptor" id #:source id))
    (define (format-id:type id)              (format-id id    "~a:type"              id #:source id))
    (define (format-id-field id field)       (format-id field "~a-~a"      id field #:source field)))
  (begin-for-syntax
    (define (format-id:type id) (format-id id "~a:type" id #:source id))))


; SYNTAX (define-record name (field ...))
;  Defines a new record type. Works like  (struct name (field ...)), but also
;  stores the field names in a record-descriptor. The record-descriptor is
;  accessable via the identifier  name-record-descriptor defined at phase 1.
(define-syntax (define-record stx)
  (syntax-parse stx
    [(_define-record record-name:id (field:id ...))
     ; construct the identifier, which will be bound to the record descriptor
     (with-syntax ([record-descriptor-id (format-id-record-descriptor #'record-name)])
       (syntax/loc stx
         (begin
           (begin-for-syntax
             ; bind the identifer to the record descriptor
             (define record-descriptor-id
               (record-descriptor
                (syntax-local-introduce #'record-name)
                ; list of field identifiers
                (map syntax-local-introduce (syntax->list #'(field ...))))))
           ; declare the corresponding structure type
           (struct record-name (field ...) #:transparent #:mutable))))]))

; SYNTAX (declare record-name id)
;   Declares that the identifier id is bound to a record-name record.
;   The declaration is stored in the form of an identifier id:type
;   bound to the record descriptor corresponding to record-name.

(define-syntax (declare stx)
  (syntax-parse stx
    [(_declare record-name id)
     ; construct the id:type identifier and the identifier to
     ; which the record descriptor is bound
     (with-syntax ([id:type                (format-id:type #'id)]
                   [name-record-descriptor (format-id-record-descriptor #'record-name)])
       (syntax/loc stx
         (begin-for-syntax
           ; bind id:type to the record descriptor
           (define id:type name-record-descriptor))))]))



; SYNTAX (dot id field)
;   If id is bound to a record value v of type r, then evaluate to (r-field v).
;   Example:        (dot man x) => (sprite-x man)
;   Implementation: The variable id must be declared as a record, so id:type
;                   is bound at phase 1 to a record descriptor that holds the field names.
;   Note:           In the final version id.field will expand to (dot id field).
(define-syntax (dot stx)
  (syntax-parse stx
    [(_dot var field)
     ;  construct identifier to which the record descriptor is bound
     ;  (if unbound then signal an error)
     (with-syntax ([var:type (format-id:type #'var)])
       ; since var:type is defined at phase 1 and we at this point also is at
       ; phase 1, we need to either use eval to reference the identifier,
       ; or use a local macro that expands into a reference.
       (syntax/loc stx
         (let-syntax
             ([insert-reference
               (λ (stx)
                 (define rd var:type)
                 ; (define rd (let-syntax ([#%top top]) var:type)) ; sigh
                 (cond
                   ; if bound, we can get the record name from the descriptor
                   [rd (define r (record-descriptor-name rd))
                       ; and now we can construct the accessor 
                       (with-syntax ([r-field (format-id-field r #'field)])
                         #'(r-field var))]
                   ; if unbound, we signal an error
                   [else #'(raise-syntax-error 'dot "identifier not declared" stx var)]))])
           (insert-reference))))]))

; SYNTAX
;  (:= id expr)       equivalent to (set! id expr)
;  (:= id field expr) equivalent to (set-name-field! id expr), if id is declared as a name record
(define-syntax (:= stx)
  (syntax-parse stx
    [(_ id:id e:expr)
     (syntax/loc stx
       (set! id expr))]
    [(_ id:id field:id e:expr)
     (with-syntax ([var:type (format-id:type #'id)])
       (syntax/loc stx
         (let-syntax
             ([insert-assignment
               (λ (so)
                 (define (format-set-name-field! name field)
                   (format-id #'id "set-~a-~a!" name field #:source field))
                 (define rd var:type)
                 (define r  (record-descriptor-name   rd))
                 (define fs (record-descriptor-fields rd))
                 (with-syntax ([set-name-field! (format-set-name-field! r #'field)])
                   (syntax/loc so
                     (set-name-field! id e))))])
           (insert-assignment))))]))

;;;
;;; TEST
;;;
     
(define-record sprite (x y bm))

(define man (sprite 1 2 'man-bitmap))

(declare sprite man)

(sprite-x man)
(dot man x)
(:= man x 11)
man

(define enemy (sprite 11 12 'enemy-bitmap))

(dot enemy x) ; this expressions gives the wrong error
              ; I was hoping for a "identifier not declared" error

