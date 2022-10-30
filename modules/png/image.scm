(define-module (png image)
  #:use-module (oop goops)
  #:use-module (rnrs bytevectors)
  #:use-module (ice-9 binary-ports)     ; put-bytevector
  #:use-module (zlib)
  #:use-module (png core common)
  #:use-module (png core chunk)
  #:use-module (png core chunk-ihdr)
  #:export (<png-raw-image>
            png-raw-image?
            png-raw-image-chunks
            png-raw-image-chunks-query

            png-image-clone
            png-image-width
            png-image-height
            png-image-bit-depth
            png-image-color-type
            png-image-data
            png-image->png

            %png-image-signature))


;; PNG image signature.
;;
;; (decimal)              137  80  78  71  13  10  26  10
;; (hexadecimal)           89  50  4e  47  0d  0a  1a  0a
;; (ASCII C notation)    \211   P   N   G  \r  \n \032 \n
;;
;; <https://www.rfc-editor.org/rfc/rfc2083#page-77>
(define %png-image-signature
  #vu8(137 80 78 71 13 10 26 10))



;; A PNG image that consists of PNG chunks.
(define-class <png-raw-image> ()
  ;; <list> of <png-chunk>
  (chunks
   #:init-value   '()
   #:init-keyword #:chunks
   #:getter       png-raw-image-chunks)

  ;; Image header.
  ;;
  ;; <png-chunk>
  (header
   #:init-value   #f
   #:getter       png-raw-image-header)

  ;; Image palette.
  ;;
  ;; <png-chunk>
  (palette
   #:init-value   #f
   #:getter       png-raw-image-palette))

(define-method (initialize (image <png-raw-image>) initargs)
  (next-method)
  (let ((ihdr-chunks (png-raw-image-chunks-query image 'IHDR)))
    (when (null? ihdr-chunks)
      (error "IHDR chunk is mandatory"))
    (slot-set! image 'header (car ihdr-chunks)))

  (let ((plte-chunks (png-raw-image-chunks-query image 'PLTE)))
    (unless (null? plte-chunks)
      (slot-set! image 'palette (car plte-chunks)))))

(define (png-raw-image? x)
  "Check if X is a PNG image instance."
  (is-a? x <png-raw-image>))

(define-method (png-image-clone (image <png-raw-image>))
  "Copy a PNG IMAGE, return a new copy."
  (make <png-raw-image>
    #:chunks (map png-chunk-clone (png-raw-image-chunks image))))



(define-method (%display (image <png-raw-image>) (port <port>))
  (let ((ihdr (png-raw-image-header image)))
    (format port "#<png-raw-image ~ax~a ~a bit ~a>"
            (png-chunk:IHDR-width ihdr)
            (png-chunk:IHDR-height ihdr)
            (png-chunk:IHDR-bit-depth ihdr)
            (object-address/hex-string image))))

(define-method (display (image <png-raw-image>) (port <port>))
  (%display image port))

(define-method (write (image <png-raw-image>) (port <port>))
  (%display image port))


(define-method (png-raw-image-chunks-query (image <png-raw-image>) (predicate <procedure>))
  (filter predicate (png-raw-image-chunks image)))

(define-method (png-raw-image-chunks-query (image <png-raw-image>) (chunk <symbol>))
  (png-raw-image-chunks-query image (lambda (c)
                                  (equal? (png-chunk-type c) chunk))))

(define-method (png-raw-image-chunks-query (image <png-raw-image>) (chunk <vector>))
  (png-raw-image-chunks-query image (lambda (c)
                                  (equal? (png-chunk-type c) chunk))))



(define-method (png-image-width (image <png-raw-image>))
  (png-chunk:IHDR-width (png-raw-image-header image)))

(define-method (png-image-height (image <png-raw-image>))
  (png-chunk:IHDR-height (png-raw-image-header image)))

(define-method (png-image-bit-depth (image <png-raw-image>))
  (png-chunk:IHDR-bit-depth (png-raw-image-header image)))

(define-method (png-image-color-type (image <png-raw-image>))
  (png-chunk:IHDR-color-type (png-raw-image-header image)))



(define-method (png-image-data (image <png-raw-image>) (uncompress? <boolean>))
  "Get the PNG image data as a single bytevector.  When UNCOMPRESS? option is
set to #t, the procedure returns data in uncompressed form."
  (let ((data-chunks (png-raw-image-chunks-query image 'IDAT)))
    (let loop ((chunks data-chunks)
               (result (if (null? data-chunks)
                           (make-bytevector 0)
                           (png-chunk-data (car data-chunks)))))
      (if (null? chunks)
          (if uncompress?
              (uncompress result)
              result)
          (let* ((chunk         (car chunks))
                 (chunk-data    (png-chunk-data chunk))
                 (result-length (bytevector-length result))
                 (chunk-length  (bytevector-length chunk-data))
                 (new-result    (make-bytevector (+ result-length chunk-length))))
            (bytevector-copy! result 0 new-result 0 result-length)
            (bytevector-copy! chunk-data 0 new-result result-length chunk-length)
            (loop (cdr chunks) new-result))))))

(define-method (png-image-data (image <png-raw-image>))
  "Get the decompressed PNG image data as a single bytevector."
  (png-image-data image #t))

(define-method (png-image->png (image <png-raw-image>) (port <output-port>))
  (put-bytevector port %png-image-signature)
  (for-each (lambda (chunk)
              (png-chunk->png chunk port))
            (png-raw-image-chunks image)))

;; image.scm ends here.
