(define-module (png core chunk-ihdr)
  #:use-module (srfi srfi-43)
  #:use-module (oop goops)
  #:use-module (png core common)
  #:use-module (png core chunk)
  #:export (<png-chunk:ihdr>
            png-chunk:ihdr-width
            png-chunk:ihdr-width-set!
            png-chunk:ihdr-height
            png-chunk:ihdr-height-set!
            png-chunk:ihdr-bit-depth
            png-chunk:ihdr-bit-depth-set!
            png-chunk:ihdr-colour-type
            png-chunk:ihdr-colour-type-set!
            png-chunk:ihdr-compression-method
            png-chunk:ihdr-compression-method-set!
            png-chunk:ihdr-filter-method
            png-chunk:ihdr-filter-method-set!
            png-chunk:ihdr-interlace-method
            png-chunk:ihdr-interlace-method-set!
            data:width
            data:heigth
            data:bit-depth
            data:colour-type
            data:compression-method
            data:filter-method
            data:interlace-method))


(define-class <png-chunk:ihdr> (<png-chunk>)
  (width
   #:init-keyword #:width
   #:getter       png-chunk:ihdr-width
   #:setter       png-chunk:ihdr-width-set!)

  (height
   #:init-keyword #:height
   #:getter       png-chunk:ihdr-height
   #:setter       png-chunk:ihdr-height-set!)

  (bit-depth
   #:init-keyword #:bit-depth
   #:getter       png-chunk:ihdr-bit-depth
   #:setter       png-chunk:ihdr-bit-depth-set!)

  (colour-type
   #:init-keyword #:colour-type
   #:getter       png-chunk:ihdr-colour-type
   #:setter       png-chunk:ihdr-colour-type-set!)

  (compression-method
   #:init-keyword #:compression-method
   #:getter       png-chunk:ihdr-compression-method
   #:setter       png-chunk:ihdr-compression-method-set!)

  (filter-method
   #:init-keyword #:filter-method
   #:getter       png-chunk:ihdr-filter-method
   #:setter       png-chunk:ihdr-filter-method-set!)

  (interlace-method
   #:init-keyword #:interlace-method
   #:getter       png-chunk:ihdr-interlace-method
   #:setter       png-chunk:ihdr-interlace-method-set!))



(define-method (%display (chunk <png-chunk:ihdr>) (port <port>))
  (let ((type (vector->chunk-type (png-chunk-type chunk))))
    (format port "#<png-chunk:ihdr ~a ~a>"
            (list-ref type 2)
            (object-address/hex-string chunk))))

(define-method (display (chunk <png-chunk:ihdr>) (port <port>))
  (%display chunk port))

(define-method (write (chunk <png-chunk:ihdr>) (port <port>))
  (%display chunk port))


(define-method (data:width (data <vector>))
  (vector->int32 (vector-copy data 0 4)))

(define-method (data:heigth (data <vector>))
  (vector->int32 (vector-copy data 4 8)))

(define-method (data:bit-depth (data <vector>))
  (vector-ref data 8))

(define-method (data:colour-type (data <vector>))
  (vector-ref data 9))

(define-method (data:compression-method (data <vector>))
  (vector-ref data 10))

(define-method (data:filter-method (data <vector>))
  (vector-ref data 11))

(define-method (data:interlace-method (data <vector>))
  (vector-ref data 12))

;;; chunk-ihdr.scm ends here.
