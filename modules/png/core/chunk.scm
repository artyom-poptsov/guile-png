;;; png-chunk.scm -- A PNG Chunk.


(define-module (png core chunk)
  #:use-module (oop goops)
  #:use-module (ice-9 iconv)
  #:use-module (png core common)
  #:use-module (png core crc)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 binary-ports)
  #:use-module (ice-9 format)
  #:use-module (rnrs bytevectors)
  #:export (<png-chunk>
            png-chunk?
            png-chunk-clone
            png-chunk-length
            png-chunk-length-set!
            png-chunk-type
            png-chunk-type-info
            png-chunk-type-set!
            png-chunk-type/description
            png-chunk-data
            png-chunk-data-set!
            png-chunk-crc
            png-chunk-crc-calculate
            png-chunk-crc-update!
            png-chunk->png
            png-chunk-valid?
            png-chunk-encode

            ;; Chunk naming.
            png-chunk-ancillary?
            png-chunk-private?
            png-chunk-safe-to-copy?

            ;; Chunk type/vector converters.
            vector->chunk-type
            chunk-type->vector

            ;; Constants.
            %png-chunk-length-bytes
            %png-chunk-type-bytes
            %png-chunk-crc-bytes

            ;; Internal procedures that should be used with extra care.
            %png-chunk-crc-set!))


(define %png-chunk-length-bytes 4)
(define %png-chunk-type-bytes   4)
(define %png-chunk-crc-bytes    4)


(define %chunk-types
  '(
    ;; 1. Critical chunk types.
    (IHDR #vu8(73 72 68 82)   "Image header")
    (PLTE #vu8(80 76 84 69)   "Palette")
    (IDAT #vu8(73 68 65 84)   "Image data")
    (IEND #vu8(73 69 78 68)   "Image trailer")

    ;; 2. Ancillary chunk types.
    (tRNS #vu8(116 82 78 83)  "Transparency")
    (cHRM #vu8(99 72 82 77)   "Primary chromaticities and white point")
    (gAMA #vu8(103 65 77 65)  "Image gamma")
    (iCCP #vu8(105 67 67 80)  "Embedded ICC profile")
    (sBIT #vu8(115 66 73 84)  "Significant bits")
    (sRGB #vu8(115 82 71 66)  "Standard RGB color space")

    ;; 2.1. Textual information.
    (tEXt #vu8(116 69 88 116) "Textual data")
    (zTXt #vu8(122 84 88 116) "Compressed textual data")
    (iTXt #vu8(105 84 88 116) "International textual data")

    ;; 2.2. Miscellaneous information.
    (bKGD #vu8(98 75 71 68)   "Background color")
    (hIST #vu8(104 73 83 84)  "Image histogram")
    (pHYs #vu8(112 72 89 115) "Physical pixel dimensions")
    (sPLT #vu8(115 80 76 84)  "Suggested palette")

    ;; 2.3. Time stamp information.
    (tIME #vu8(116 73 77 69)  "Image last-modification time")

    ;; Exif.
    ;; <https://github.com/corkami/formats/blob/master/image/png.md>
    (eXIf #vu8(101 88  73 102) "EXIF - registered July 2017")
    (zxIf #vu8(122 120 73 102) "compressed EXIF")
    (vpAg #vu8(118 112 65 103) "VirtualPage (deprecated)")
    (caNv #vu8(99  97  78 118) "Canvas (superceding)")

    ;; ImageMagick private chunks.
    (orNT #vu8(111 114 78 84) "ImageMagick: Image orientation")
    (vpAg #vu8(118 112 65 103) "ImageMagick: Virtual page")
    ))




(define-class <png-chunk> ()
  ;; A 4-byte unsigned integer giving the number of bytes in the chunk's data
  ;; field. The length counts only the data field, not itself, the chunk type
  ;; code, or the CRC.  Zero is a valid length.  The value must not exceed
  ;; (2^31)-1 bytes.
  ;;
  ;; <number>
  (length
   #:init-value   0
   #:init-keyword #:length
   #:getter       png-chunk-length
   #:setter       png-chunk-length-set!)

  ;; A chunk type code.  For convenience in description and in examining PNG
  ;; files, type codes are restricted to consist of uppercase and lowercase
  ;; ASCII letters (A-Z and a-z, or 65-90 and 97-122 decimal).
  ;;
  ;; <symbol> | #f
  (type
   #:init-value   #f
   #:init-keyword #:type
   #:getter       png-chunk-type
   #:setter       png-chunk-type-set!)

  ;; The chunk data.  This part can be of zero length.
  ;;
  ;; <vector>
  (data
   #:init-thunk   (lambda () (make-bytevector 0))
   #:init-keyword #:data
   #:getter       png-chunk-data
   #:setter       png-chunk-data-set!)

  ;; 4-byte CRC Cyclic Redundancy Check) calculated on the preceding bytes in
  ;; the chunk, including the chunk type code and chunk data fields, but not
  ;; including the length field.  The CRC is always present, even for chunks
  ;; containing no data.
  ;;
  ;; <number>
  (crc
   #:init-value   0
   #:init-keyword #:crc
   #:getter       png-chunk-crc
   #:setter       %png-chunk-crc-set!))

(define (png-chunk? x)
  (is-a? x <png-chunk>))

(define-method (equal? (chunk1 <png-chunk>) (chunk2 <png-chunk>))
  "Compare CHUNK1 with CHUNK2."
  (and (equal? (png-chunk-length chunk1) (png-chunk-length chunk2))
       (equal? (png-chunk-crc chunk1) (png-chunk-crc chunk2))))

(define-method (png-chunk-clone (chunk <png-chunk>))
  (make <png-chunk>
    #:type   (png-chunk-type chunk)
    #:length (png-chunk-length chunk)
    #:data   (png-chunk-data chunk)
    #:crc    (png-chunk-crc chunk)))

(define-method (vector->chunk-type (vec <bytevector>))
  "Convert a vector VEC with PNG chunk type to a PNG chunk type list.  Return
the list."
  (let loop ((types %chunk-types))
    (if (null? types)
        #f
        (if (equal? (list-ref (car types) 1) vec)
            (car types)
            (loop (cdr types))))))

(define-method (png-chunk-type-info (type <symbol>))
  (find (lambda (t)
          (equal? (car t) type))
        %chunk-types))

(define-method (chunk-type->vector (type <symbol>))
  "Convert a PNG chunk TYPE to a vector.  Return the vector."
  (let ((info (png-chunk-type-info type)))
    (if info
        (cadr (png-chunk-type-info type))
        (string->bytevector (symbol->string type) "ASCII"))))

(define-method (png-chunk-type-info (chunk <png-chunk>))
  (png-chunk-type-info (png-chunk-type chunk)))



(define-method (png-chunk-ancillary? (bv <bytevector>))
  "Check if a bytevector BV describes a chunk type that is ancillary."
  (> (logand (bytevector-u8-ref bv 0) 32) 0))

(define-method (png-chunk-ancillary? (chunk <png-chunk>))
  "Check if a CHUNK is ancillary."
  (let ((type-info (png-chunk-type-info (png-chunk-type chunk))))
    (png-chunk-ancillary? (list-ref type-info 1))))

(define-method (png-chunk-private? (bv <bytevector>))
  "Check if a bytevector BV describes a chunk type that is private."
  (> (logand (bytevector-u8-ref bv 1) 32) 0))

(define-method (png-chunk-private? (chunk <png-chunk>))
  "Check if a CHUNK is private."
  (let ((type-info (png-chunk-type-info (png-chunk-type chunk))))
    (png-chunk-private? (list-ref type-info 1))))

(define-method (png-chunk-safe-to-copy? (bv <bytevector>))
  "Check if a bytevector BV describes a chunk type that is safe to copy."
  (zero? (logand (bytevector-u8-ref bv 3) 32)))

(define-method (png-chunk-safe-to-copy? (chunk <png-chunk>))
  "Check if a CHUNK is safe to copy."
  (let ((type-info (png-chunk-type-info (png-chunk-type chunk))))
    (png-chunk-safe-to-copy? (list-ref type-info 1))))



(define-method (%display (chunk <png-chunk>) (port <port>))
  (let* ((type      (png-chunk-type chunk))
         (type-info (png-chunk-type-info type)))
    (if type-info
        (format port "#<png-chunk type: ~a (~a) length: ~a crc: ~X ~a>"
                type
                (list-ref type-info 2)
                (png-chunk-length chunk)
                (png-chunk-crc chunk)
                (object-address/hex-string chunk))
        (format port "#<png-chunk type: ~a length: ~a crc: ~X ~a>"
                type
                (png-chunk-length chunk)
                (png-chunk-crc chunk)
                (object-address/hex-string chunk)))))

(define-method (display (chunk <png-chunk>) (port <port>))
  (%display chunk port))

(define-method (write (chunk <png-chunk>) (port <port>))
  (%display chunk port))



(define-method (png-chunk-type/description (chunk <png-chunk>))
  "Get the description of the PNG CHUNK type.  Return the description as a
string."
  (list-ref (png-chunk-type-info (png-chunk-type chunk)) 2))

(define-method (png-chunk-crc-calculate (chunk <png-chunk>))
  (let* ((type        (chunk-type->vector (png-chunk-type chunk)))
         (data        (png-chunk-data chunk))
         (data-length (bytevector-length data))
         (bv          (make-bytevector (+ %png-chunk-type-bytes data-length)
                                       0)))
    (bytevector-copy! type 0 bv 0 %png-chunk-type-bytes)
    (bytevector-copy! data 0 bv %png-chunk-type-bytes data-length)
    (crc bv)))

(define-method (png-chunk-crc-update! (chunk <png-chunk>))
  "Update a PNG CHUNK so the CRC will match the chunk content."
  (%png-chunk-crc-set! chunk (png-chunk-crc-calculate chunk)))

(define-method (png-chunk-valid? (chunk <png-chunk>))
  "Check if CRC of a CHUNK is valid."
  (equal? (png-chunk-crc-calculate chunk) (png-chunk-crc chunk)))



(define-method (png-chunk->png (chunk <png-chunk>) (port <output-port>))
  "Print a PNG CHUNK to a binary PORT."
  (put-bytevector port (int32->bytevector (png-chunk-length chunk)))
  (put-bytevector port (chunk-type->vector (png-chunk-type chunk)))
  (put-bytevector port (png-chunk-data chunk))
  (put-bytevector port (int32->bytevector (png-chunk-crc chunk))))

(define-generic png-chunk-encode)

;;; png-chunk.scm ends here.
