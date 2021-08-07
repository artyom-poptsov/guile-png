(define-module (png core common)
  #:use-module (oop goops)
  #:export (vector->int32))

(define-method (vector->int32 (v <vector>))
  (logior (ash (vector-ref v 0) 24)
          (ash (vector-ref v 1) 16)
          (ash (vector-ref v 2) 8)
          (vector-ref v 3)))
