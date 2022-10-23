(define-module (png core common)
  #:use-module (oop goops)
  #:use-module (rnrs bytevectors)
  #:export (object-address/hex-string
            vector->int32
            vector->int16
            int32->bytevector
            bytevector-copy/part))


(define (object-address/hex-string object)
  (number->string (object-address object) 16))


(define-method (vector->int32 (v <bytevector>))
  (logior (ash (bytevector-u8-ref v 0) 24)
          (ash (bytevector-u8-ref v 1) 16)
          (ash (bytevector-u8-ref v 2) 8)
          (bytevector-u8-ref v 3)))

(define-method (vector->int16 (v <bytevector>))
  (logior (ash (bytevector-u8-ref v 0) 8)
          (bytevector-u8-ref v 1)))

(define-method (int32->bytevector (number <number>))
  "Convert a NUMBER to a byte vector."
  (u8-list->bytevector
   (list (ash (logand number #xFF000000) -24)
         (ash (logand number #x00FF0000) -16)
         (ash (logand number #x0000FF00) -8)
         (logand number #x000000FF))))

(define-method (bytevector-copy/part bv source-start length)
  (let ((result (make-bytevector length)))
    (bytevector-copy! bv source-start result 0 length)
    result))
