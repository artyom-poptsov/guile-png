(define-module (png core chunk-phys)
  #:use-module (srfi srfi-43)
  #:use-module (oop goops)
  #:use-module (png core common)
  #:use-module (png core chunk)
  #:export (<png-chunk:pHYs>
            png-chunk:pHYs-pixels-per-unit-x-axis
            png-chunk:pHYs-pixels-per-unit-y-axis
            png-chunk:pHYs-unit-specifier
            data->png-chunk:pHYs))



(define-class <png-chunk:pHYs> (<png-chunk>)
  ;; <number>
  (pixels-per-unit-x-axis
   #:init-keyword #:pixels-per-unit-x-axis
   #:getter       png-chunk:pHYs-pixels-per-unit-x-axis)

  ;; <number>
  (pixels-per-unit-y-axis
   #:init-keyword #:pixels-per-unit-y-axis
   #:getter       png-chunk:pHYs-pixels-per-unit-y-axis)

  ;; <number>
  (unit-specifier
   #:init-keyword #:unit-specifier
   #:getter       png-chunk:pHYs-unit-specifier))



(define-method (%display (chunk <png-chunk:pHYs>) (port <port>))
  (let ((type (vector->chunk-type (png-chunk-type chunk))))
    (format port "#<png-chunk:pHYs ~a: ~ax~a (unit: ~a) ~a>"
            (list-ref type 2)
            (png-chunk:pHYs-pixels-per-unit-x-axis chunk)
            (png-chunk:pHYs-pixels-per-unit-y-axis chunk)
            (if (zero? (png-chunk:pHYs-unit-specifier chunk))
                "unknown"
                "metre")
            (object-address/hex-string chunk))))

(define-method (display (chunk <png-chunk:pHYs>) (port <port>))
  (%display chunk port))

(define-method (write (chunk <png-chunk:pHYs>) (port <port>))
  (%display chunk port))

(define-method (data->png-chunk:pHYs (data   <vector>)
                                     (type   <vector>)
                                     (length <number>)
                                     (crc    <vector>))
  (make <png-chunk:pHYs>
    #:length length
    #:type   type
    #:data   data
    #:crc    crc
    #:pixels-per-unit-x-axis (vector->int32 (vector-copy data 0 4))
    #:pixels-per-unit-y-axis (vector->int32 (vector-copy data 4 8))
    #:unit-specifier         (vector-ref data 8)))


