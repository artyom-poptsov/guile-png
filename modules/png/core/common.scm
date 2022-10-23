(define-module (png core common)
  #:use-module (oop goops)
  #:export (object-address/hex-string
            vector->int32
            vector->int16
            int32->bytevector))


(define (object-address/hex-string object)
  (number->string (object-address object) 16))


(define-method (vector->int32 (v <vector>))
  (logior (ash (vector-ref v 0) 24)
          (ash (vector-ref v 1) 16)
          (ash (vector-ref v 2) 8)
          (vector-ref v 3)))

(define-method (vector->int16 (v <vector>))
  (logior (ash (vector-ref v 0) 8)
          (vector-ref v 1)))

(define-method (int32->bytevector (number <number>))
  "Convert a NUMBER to a byte vector."
  (u8-list->bytevector
   (list (ash (logand v #xFF000000) -24)
         (ash (logand v #x00FF0000) -16)
         (ash (logand v #x0000FF00) -8)
         (logand v #x000000FF))))

