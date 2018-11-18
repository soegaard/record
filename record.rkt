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

(require (for-syntax syntax/parse racket/syntax racket/list)
         (for-syntax "descriptor.rkt" "top.rkt")
         (for-syntax (for-syntax racket/base)))

;;; Helpers used to construct indentifiers.

(begin-for-syntax
  (define (format-id-record-descriptor id) (format-id id "~a-record-descriptor" id #:source id))
  (define (format-id:type id)              (format-id id "~a:type"              id #:source id))
  (define (format-id-field id fd)          (format-id fd "~a-~a"             id fd #:source fd))
  (define (format-set-id-field! id fd)     (format-id id "set-~a-~a!"        id fd #:source fd)))


;;; Helpers handling identifiers containing a dot aka dotted identifiers

; dotted? : syntax -> boolean
;   does the identifier x contains a period (.) ?
(begin-for-syntax
  (require racket/string)
  
  (define (dotted? x)
    (or (and (identifier? x) (dotted? (syntax-e x)))
        (and (symbol? x)     (dotted? (symbol->string x)))
        (and (string? x)     (string-contains? x "."))))
  
  (define (split-dotted-identifier id)
    (define str (symbol->string (syntax-e id)))
    (define xs  (string-split str "."))
    (for/list ([x xs])
      (datum->syntax id (string->symbol x) id))))

; SYNTAX-CLASS
;   A  dotted-identifier  contains a period.
;   The attribute  ids  holds a list of subidentifiers.
;   For the dotted identifier foo.bar, the attribute  ids  stores (foo bar).
(begin-for-syntax
  (define-syntax-class dotted-id
    #:attributes (ids) ; ids stores a list of subidentifiers, e.g. foo.bar turns into (foo bar)
    #:description "dotted identifier" ; used in automatically generated error messages
    #:opaque                          ; errors does not use internal structure
    ; (pattern syntax-pattern syntax-directive)
    (pattern id:id
             #:fail-unless (dotted? #'id) "expected a dotted identifier"
             #:attr ids (split-dotted-identifier #'id)))) ; store the subids in ids

; SYNTAX-CLASS
(begin-for-syntax
  (define-syntax-class field-spec
    #:description "field zpecification"
    ; Note: The attribute type is #f, when no type was given.
    (pattern (~or* field:id [field:id type:id]))))
             

; SYNTAX (define-record name (field ...))
;  Defines a new record type. Works like  (struct name (field ...)), but also
;  stores the field names in a record-descriptor. The record-descriptor is
;  accessable via the identifier  name-record-descriptor defined at phase 1.
(define-syntax (define-record stx)
  (syntax-parse stx
    [(_define-record record-name:id (fs:field-spec ...))
     ; type->descriptor : id-or-#f -> record-descriptor-or-#f
     ;   If t is the name of a record, the corresponding record descriptor is returned,
     ;   otherwise #f is returned.
     (define (type->descriptor t)
       (syntax-parse t
         [#f #f]
         [_  (syntax-local-value (format-id-record-descriptor t) (λ () #f))]))
     ; construct the identifier, which will be bound to the record descriptor
     (with-syntax ([(field ...)          (attribute fs.field)]
                   [(type  ...)          (attribute fs.type)] ; list of id-or-#f
                   [record-descriptor-id (format-id-record-descriptor #'record-name)]
                   [(desc ...)           (map type->descriptor (attribute fs.type))])
       ; (displayln #'(field ...))
       ; (displayln #'(type  ...))
       (quasisyntax/loc stx
         (begin
           ; bind the identifer to the record descriptor
           (define-syntax record-descriptor-id
             (record-descriptor
              ; name
              (syntax-local-introduce #'record-name)
              ; list of field identifiers
              (map syntax-local-introduce (syntax->list #'(field ...)))
              ; list of types
              (list desc ...)))
           ; declare the corresponding structure type
           (struct record-name (field ...) #:transparent #:mutable))))]))

; SYNTAX (declare record-name id)
;   Declares that the identifier  id  is bound to a record-name record.
;   The declaration is stored in the form of an identifier  id:type
;   bound to the record descriptor corresponding to record-name.

(define-syntax (declare stx)
  (syntax-parse stx
    [(_declare record-name id)
     ; construct the id:type identifier and the identifier to
     ; which the record descriptor is bound
     (with-syntax ([id:type                (format-id:type #'id)]
                   [name-record-descriptor (format-id-record-descriptor #'record-name)])
       (syntax/loc stx
         (begin
           ; bind id:type to the record descriptor
           (define-syntax id:type (syntax-local-value #'name-record-descriptor)))))]))

; SYNTAX (dot id field)
;   If id is bound to a record value v of type r, then evaluate to (r-field v).
;   Example:        (dot man x) => (sprite-x man)
;   Implementation: The variable id must be declared as a record, so id:type
;                   is bound at phase 1 to a record descriptor that holds the field names.
;   Note:           In the final version id.field will expand to (dot id field).
(define-syntax (dot stx)
  ; handle-dot
  ;   produce expressions to access  expr.field0.field1. ...
  ;   where fields = (field0 ...) and rd is the record-descriptor of expr
  (define (field-index rd field)
    (index-of (record-descriptor-fields rd) field
              (λ (x y) (and x y (eq? (syntax-e x) (syntax-e y))))))
  (define (handle-dot expr rd fields) 
    (syntax-parse fields
      [() expr]
      [(field fields ...)
       (with-syntax ([expr expr])
         (cond
           ; if bound, we can get the record name from the descriptor
           [rd (define r (record-descriptor-name rd))
               ; and now we can construct the accessor 
               (define expr.field
                 (with-syntax ([r-field (format-id-field r #'field)])
                   (syntax/loc stx
                     (r-field expr))))
               (define i        (field-index rd #'field))
               ;(displayln (list 'index rd #'field i))
               (define rd.field (list-ref (record-descriptor-types rd) i))
               (handle-dot expr.field rd.field #'(fields ...))]
           ; if unbound, we signal an error
           [else (raise-syntax-error 'dot "identifier not declared as a record" stx #'expr)]))]))
  (syntax-parse stx
    [(_dot var field ...)
     ;  construct identifier to which the record descriptor is bound
     ;  (if unbound then signal an error)
     (with-syntax ([var:type (format-id:type #'var)])
       (define rd (syntax-local-value #'var:type (λ () #f)))
       (handle-dot #'var rd #'(field ...)))]))


; SYNTAX
;  (:= id expr)       equivalent to (set! id expr)
;  (:= id field expr) equivalent to (set-name-field! id expr), if id is declared as a name record
(define-syntax (:= stx)
  (syntax-parse stx
    ; handled dotted identifiers before non-dotted identifiers
    [(_ d:dotted-id e:expr)
     (with-syntax ([(id field ...) (attribute d.ids)])
       (syntax/loc stx
         (:= id field ... e)))]
    [(_ id:id e:expr)
     (syntax/loc stx
       (set! id e))]    
    [(_ id:id field:id e:expr)
     ; TODO: extend to more than one field
     (with-syntax ([var:type (format-id:type #'id)])       
       (define rd (syntax-local-value #'var:type (λ () #f)))
       (cond
         [rd (define r  (record-descriptor-name   rd))
             (define fs (record-descriptor-fields rd))
             (with-syntax ([set-id-field! (format-set-id-field! r #'field)])
               (syntax/loc stx
                 (set-id-field! id e)))]
         [else
          (raise-syntax-error ':= "identifier not declared as a record" stx #'id)]))]))



;;;
;;; TEST
;;;
     
(define-record sprite (x y bm))
(define-record star   (color [sprite sprite]))

(define man (sprite 1 2 'man-bitmap))

(declare sprite man)

(sprite-x man)
(dot man x)
(:= man x 11)
(:= man.x 22)
man

(define enemy (sprite 11 12 'enemy-bitmap))
(define a-star (star 'red (sprite 22 33 'star-bitmap)))
; (dot enemy x) 
; (:= man foo 12)

(declare star a-star)
(dot a-star color)
(dot a-star sprite x)

