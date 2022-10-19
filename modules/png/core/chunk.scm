;;; png-chunk.scm -- A PNG Chunk.


(define-module (png core chunk)
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
            %png-chunk-crc-bytes))


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

  ;; A 4-byte chunk type code.  For convenience in description and in
  ;; examining PNG files, type codes are restricted to consist of uppercase
  ;; and lowercase ASCII letters (A-Z and a-z, or 65-90 and 97-122 decimal).
  ;; <vector>
  (type
   #:init-thunk   (lambda () (make-vector 0))
   #:init-keyword #:type
   #:getter       png-chunk-type
   #:setter       png-chunk-type-set!)

  ;; The chunk data.  This part can be of zero length.
  ;;
  ;; <vector>
  (data
   #:init-thunk   (lambda () (make-vector 0))
   #:init-keyword #:data
   #:getter       png-chunk-data
   #:setter       png-chunk-data-set!)

  ;; 4-byte CRC Cyclic Redundancy Check) calculated on the preceding bytes in
  ;; the chunk, including the chunk type code and chunk data fields, but not
  ;; including the length field.  The CRC is always present, even for chunks
  ;; containing no data.
  ;;
  ;; <vector>
  (crc
   #:init-thunk   (lambda () (make-vector 0))
   #:init-keyword #:crc
   #:getter       png-chunk-crc
   #:setter       png-chunk-crc-set!)

  #:metaclass <redefinable-class>)



(define-method (%display (chunk <png-chunk>) (port <port>))
  (let ((type (vector->chunk-type (png-chunk-type chunk))))
    (format port "#<png-chunk:~a ~a ~a>"
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

;;; png-chunk.scm ends here.
