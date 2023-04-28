(define-module (png fsm signature-context)
  #:use-module (oop goops)
  #:use-module (ice-9 binary-ports)
  #:use-module (png fsm context)
  #:re-export (guard:#t
               action:no-op
               u8:letter-P?
               u8:letter-N?
               u8:letter-G?
               u8:cr?
               u8:lf?)
  #:export (<signature-context>
            guard:correct-first-byte?
            guard:letter-ctrl-z?

            event-source

            action:wrong-first-byte-error
            action:unexpected-eof-error
            action:unexpected-byte-error))

(define-class <signature-context> (<u8-context>))

(define event-source binary-context-event-source)


(define-public (guard:correct-first-byte? ctx byte)
  (equal? byte 137))

(define-public (guard:letter-ctrl-z? ctx byte)
  (u8:sub? ctx byte))


(define (action:wrong-first-byte-error ctx byte)
  (error "Wrong first byte" ctx byte))

(define (action:unexpected-eof-error ctx byte)
  (error "Unexpected end of file" ctx byte))

(define (action:unexpected-byte-error ctx byte)
  (error "Unexpected byte read" ctx byte))
