(define-module (png core chunk-chrm)
  #:use-module (srfi srfi-43)
  #:use-module (oop goops)
  #:use-module (png core common)
  #:use-module (png core chunk)
  #:export (<png-chunk:cHRM>
            png-chunk:cHRM-white-point-x
            png-chunk:cHRM-white-point-y
            png-chunk:cHRM-red-x
            png-chunk:cHRM-red-y
            png-chunk:cHRM-green-x
            png-chunk:cHRM-green-y
            png-chunk:cHRM-blue-x
            png-chunk:cHRM-blue-y

            data:white-point-x
            data:white-point-y
            data:red-x
            data:red-y
            data:green-x
            data:green-y
            data:blue-x
            data:blue-y))



(define-class <png-chunk:cHRM> (<png-chunk>)
  (white-point-x
   #:init-keyword #:white-point-x
   #:getter       png-chunk:cHRM-white-point-x)

  (white-point-y
   #:init-keyword #:white-point-y
   #:getter       png-chunk:cHRM-white-point-y)

  (red-x
   #:init-keyword #:red-x
   #:getter       png-chunk:cHRM-red-x)

  (red-y
   #:init-keyword #:red-y
   #:getter       png-chunk:cHRM-red-y)

  (green-x
   #:init-keyword #:green-x
   #:getter       png-chunk:cHRM-green-x)

  (green-y
   #:init-keyword #:green-y
   #:getter       png-chunk:cHRM-green-y)

  (blue-x
   #:init-keyword #:blue-x
   #:getter       png-chunk:cHRM-blue-x)

  (blue-y
   #:init-keyword #:blue-y
   #:getter       png-chunk:cHRM-blue-y))

(define-method (%display (chunk <png-chunk:cHRM>) (port <port>))
  (let ((type (vector->chunk-type (png-chunk-type chunk))))
    (format port "#<png-chunk:cHRM ~a ~a>"
            (list-ref type 2)
            (object-address/hex-string chunk))))

(define-method (display (chunk <png-chunk:cHRM>) (port <port>))
  (%display chunk port))

(define-method (write (chunk <png-chunk:cHRM>) (port <port>))
  (%display chunk port))



(define-method (data:white-point-x (vec <vector>))
  (vector->int32 (vector-copy vec 0 4)))

(define-method (data:white-point-y (vec <vector>))
  (vector->int32 (vector-copy vec 4 8)))

(define-method (data:red-x (vec <vector>))
  (vector->int32 (vector-copy vec 8 12)))

(define-method (data:red-y (vec <vector>))
  (vector->int32 (vector-copy vec 12 16)))

(define-method (data:green-x (vec <vector>))
  (vector->int32 (vector-copy vec 16 20)))

(define-method (data:green-y (vec <vector>))
  (vector->int32 (vector-copy vec 20 24)))

(define-method (data:blue-x (vec <vector>))
  (vector->int32 (vector-copy vec 24 28)))

(define-method (data:blue-y (vec <vector>))
  (vector->int32 (vector-copy vec 28 32)))

;;; chunk-iend.scm ends here.
