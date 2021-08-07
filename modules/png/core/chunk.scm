;;; png-chunk.scm -- A PNG Chunk.


(define-module (png core chunk)
  #:use-module (srfi srfi-43)
  #:use-module (oop goops)
  #:use-module (png core common)
  #:export (<png-chunk>
            png-chunk-length
            png-chunk-length-set!
            png-chunk-type
            png-chunk-type-set!
            png-chunk-type/name
            png-chunk-type/description
            png-chunk-data
            png-chunk-data-set!
            png-chunk-crc
            png-chunk-crc-set!

            ;; Chunk type/vector converters.
            vector->chunk-type
            chunk-type->vector

            ;; Constants.
            %png-chunk-length-bytes
            %png-chunk-type-bytes
            %png-chunk-crc-bytes

            png-chunk->typed-chunk

            ;; Decoded chunks.
            <png-chunk:ihdr>
            data:width
            data:heigth
            data:bit-depth
            data:colour-type
            data:compression-method
            data:filter-method
            data:interlace-method))


(define %png-chunk-length-bytes 4)
(define %png-chunk-type-bytes   4)
(define %png-chunk-crc-bytes    4)


(define %chunk-types
  '(
    ;; 1. Critical chunk types.
    (IHDR #(73 72 68 82)   "Image header")
    (PLTE #(80 76 84 69)   "Palette")
    (IDAT #(73 68 65 84)   "Image data")
    (IEND #(73 69 78 68)   "Image trailer")

    ;; 2. Ancillary chunk types.
    (tRNS #(116 82 78 83)  "Transparency")
    (cHRM #(99 72 82 77)   "Primary chromaticities and white point")
    (gAMA #(103 65 77 65)  "Image gamma")
    (iCCP #(105 67 67 80)  "Embedded ICC profile")
    (sBIT #(115 66 73 84)  "Significant bits")
    (sRGB #(115 82 71 66)  "Standard RGB colour space")

    ;; 2.1. Textual information.
    (tEXT #(116 69 88 116) "Textual data")
    (zTXt #(122 84 88 116) "Compressed textual data")
    (iTXt #(105 84 88 116) "International textual data")

    ;; 2.2. Miscellaneous information.
    (bKGD #(98 75 71 68)   "Background colour")
    (hIST #(104 73 83 84)  "Image histogram")
    (pHYs #(112 72 89 115) "Physical pixel dimensions")
    (sPLT #(115 80 76 84)  "Suggested palette")

    ;; 2.3. Time stamp information.
    (tIME #(116 73 77 69)  "Image last-modification time")
    ))

(define-method (vector->chunk-type (vec <vector>))
  (let loop ((types %chunk-types))
    (if (null? types)
        #f
        (if (equal? (list-ref (car types) 1) vec)
            (car types)
            (loop (cdr types))))))

(define-method (chunk-type->vector (type <symbol>))
  (list-ref (member %chunk-types type) 1))

  
  (define-class <png-chunk> ()
    ;; <number>
  (length
   #:init-keyword #:length
   #:getter       png-chunk-length
   #:setter       png-chunk-length-set!)

  ;; <vector>
  (type
   #:init-keyword #:type
   #:getter       png-chunk-type
   #:setter       png-chunk-type-set!)

  ;; <vector>
  (data
   #:init-keyword #:data
   #:getter       png-chunk-data
   #:setter       png-chunk-data-set!)

  ;; <vector>
  (crc
   #:init-keyword #:crc
   #:getter       png-chunk-crc
   #:setter       png-chunk-crc-set!))


(define (object-address/hex-string object)
  (number->string (object-address object) 16))

(define-method (%display (chunk <png-chunk>) (port <port>))
  (let ((type (vector->chunk-type (png-chunk-type chunk))))
    (format port "#<png-chunk ~a ~a: ~a ~a>"
            (png-chunk-type chunk)
            (list-ref type 0)
            (list-ref type 2)
            (object-address/hex-string chunk))))

(define-method (display (chunk <png-chunk>) (port <port>))
  (%display chunk port))

(define-method (write (chunk <png-chunk>) (port <port>))
  (%display chunk port))



(define-method (png-chunk-type/name (chunk <png-chunk>))
  (let ((type (vector->chunk-type (png-chunk-type chunk))))
    (and type
         (list-ref type 0))))

(define-method (png-chunk-type/description (chunk <png-chunk>))
  (let ((type (vector->chunk-type (png-chunk-type chunk))))
    (and type
         (list-ref type 2))))


;;; IHDR chunk.

(define-class <png-chunk:ihdr> (<png-chunk>)
  (width
   #:init-keyword #:width
   #:getter       png-chunk-ihdr-width
   #:setter       png-chunk-ihdr-width-set!)

  (height
   #:init-keyword #:height
   #:getter       png-chunk-ihdr-height
   #:setter       png-chunk-ihdr-height-set!)

  (bit-depth
   #:init-keyword #:bit-depth
   #:getter       png-chunk-ihdr-bit-depth
   #:setter       png-chunk-ihdr-bit-depth-set!)

  (colour-type
   #:init-keyword #:colour-type
   #:getter       png-chunk-colour-type
   #:setter       png-chunk-colour-type-set!)

  (compression-method
   #:init-keyword #:compression-method
   #:getter       png-chunk-compression-method
   #:setter       png-chunk-compression-method-set!)

  (filter-method
   #:init-keyword #:filter-method
   #:getter       png-chunk-filter-method
   #:setter       png-chunk-filter-method-set!)

  (interlace-method
   #:init-keyword #:interlace-method
   #:getter       png-chunk-interlace-method
   #:setter       png-chunk-interlace-method-set!))

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

;;; png-chunk.scm ends here.
