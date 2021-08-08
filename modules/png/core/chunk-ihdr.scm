(define-module (png core chunk-ihdr)
  #:use-module (srfi srfi-43)
  #:use-module (oop goops)
  #:use-module (png core common)
  #:use-module (png core chunk)
  #:export (<png-chunk:IHDR>
            png-chunk:IHDR-width
            png-chunk:IHDR-width-set!
            png-chunk:IHDR-height
            png-chunk:IHDR-height-set!
            png-chunk:IHDR-bit-depth
            png-chunk:IHDR-bit-depth-set!
            png-chunk:IHDR-colour-type
            png-chunk:IHDR-colour-type-set!
            png-chunk:IHDR-compression-method
            png-chunk:IHDR-compression-method-set!
            png-chunk:IHDR-filter-method
            png-chunk:IHDR-filter-method-set!
            png-chunk:IHDR-interlace-method
            png-chunk:IHDR-interlace-method-set!
            data:width
            data:heigth
            data:bit-depth
            data:colour-type
            data:compression-method
            data:filter-method
            data:interlace-method))


(define-class <png-chunk:IHDR> (<png-chunk>)
  (width
   #:init-keyword #:width
   #:getter       png-chunk:IHDR-width
   #:setter       png-chunk:IHDR-width-set!)

  (height
   #:init-keyword #:height
   #:getter       png-chunk:IHDR-height
   #:setter       png-chunk:IHDR-height-set!)

  (bit-depth
   #:init-keyword #:bit-depth
   #:getter       png-chunk:IHDR-bit-depth
   #:setter       png-chunk:IHDR-bit-depth-set!)

  (colour-type
   #:init-keyword #:colour-type
   #:getter       png-chunk:IHDR-colour-type
   #:setter       png-chunk:IHDR-colour-type-set!)

  (compression-method
   #:init-keyword #:compression-method
   #:getter       png-chunk:IHDR-compression-method
   #:setter       png-chunk:IHDR-compression-method-set!)

  (filter-method
   #:init-keyword #:filter-method
   #:getter       png-chunk:IHDR-filter-method
   #:setter       png-chunk:IHDR-filter-method-set!)

  (interlace-method
   #:init-keyword #:interlace-method
   #:getter       png-chunk:IHDR-interlace-method
   #:setter       png-chunk:IHDR-interlace-method-set!))



(define-method (%display (chunk <png-chunk:IHDR>) (port <port>))
  (let ((type (vector->chunk-type (png-chunk-type chunk))))
    (format port "#<png-chunk:IHDR ~a ~a>"
            (list-ref type 2)
            (object-address/hex-string chunk))))

(define-method (display (chunk <png-chunk:IHDR>) (port <port>))
  (%display chunk port))

(define-method (write (chunk <png-chunk:IHDR>) (port <port>))
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
