(define-module (png image)
  #:use-module (oop goops)
  #:use-module (rnrs bytevectors)
  #:use-module (ice-9 binary-ports)     ; put-bytevector
  #:use-module (zlib)
  #:use-module (png core common)
  #:use-module (png core chunk)
  #:use-module (png core chunk-ihdr)
  #:use-module (png core chunk-iend)
  #:use-module (png chunk-converter)
  #:export (<png-compressed-image>
            png-compressed-image?
            png-compressed-image-decompress

            <png-image>
            png-image-chunks
            png-image-chunks-query
            png-image-clone
            png-image-width
            png-image-height
            png-image-bit-depth
            png-image-color-type
            png-image-data
            png-image->png
            png-image-compress

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
(define-class <png-compressed-image> ()
  ;; <list> of <png-chunk>
  (chunks
   #:init-value   '()
   #:init-keyword #:chunks
   #:getter       png-image-chunks)

  ;; Image header.
  ;;
  ;; <png-chunk>
  (header
   #:init-value   #f
   #:getter       png-image-header)

  ;; Image palette.
  ;;
  ;; <png-chunk>
  (palette
   #:init-value   #f
   #:getter       png-image-palette))

(define-method (initialize (image <png-compressed-image>) initargs)
  (next-method)
  (let ((ihdr-chunks (png-image-chunks-query image 'IHDR)))
    (when (null? ihdr-chunks)
      (error "IHDR chunk is mandatory"))
    (slot-set! image 'header (car ihdr-chunks)))

  (let ((plte-chunks (png-image-chunks-query image 'PLTE)))
    (unless (null? plte-chunks)
      (slot-set! image 'palette (car plte-chunks)))))

(define (png-compressed-image? x)
  "Check if X is a PNG image instance."
  (is-a? x <png-compressed-image>))

(define-method (png-image-clone (image <png-compressed-image>))
  "Copy a PNG IMAGE, return a new copy."
  (make <png-compressed-image>
    #:chunks (map png-chunk-clone (png-image-chunks image))))



(define-method (%display (image <png-compressed-image>) (port <port>))
  (let ((ihdr (png-image-header image)))
    (format port "#<png-compressed-image ~ax~a ~a bit ~a>"
            (png-chunk:IHDR-width ihdr)
            (png-chunk:IHDR-height ihdr)
            (png-chunk:IHDR-bit-depth ihdr)
            (object-address/hex-string image))))

(define-method (display (image <png-compressed-image>) (port <port>))
  (%display image port))

(define-method (write (image <png-compressed-image>) (port <port>))
  (%display image port))


(define-method (png-image-chunks-query (chunks <list>) (chunk <symbol>))
  (filter (lambda (c) (equal? (png-chunk-type c) chunk)) chunks))

(define-method (png-image-chunks-query (image <png-compressed-image>) (predicate <procedure>))
  (filter predicate (png-image-chunks image)))

(define-method (png-image-chunks-query (image <png-compressed-image>) (chunk <symbol>))
  (png-image-chunks-query image (lambda (c)
                                  (equal? (png-chunk-type c) chunk))))

(define-method (png-image-chunks-query (image <png-compressed-image>) (chunk <vector>))
  (png-image-chunks-query image (lambda (c)
                                  (equal? (png-chunk-type c) chunk))))



(define-method (png-image-width (image <png-compressed-image>))
  (png-chunk:IHDR-width (png-image-header image)))

(define-method (png-image-height (image <png-compressed-image>))
  (png-chunk:IHDR-height (png-image-header image)))

(define-method (png-image-bit-depth (image <png-compressed-image>))
  (png-chunk:IHDR-bit-depth (png-image-header image)))

(define-method (png-image-color-type (image <png-compressed-image>))
  (png-chunk:IHDR-color-type (png-image-header image)))



(define-method (png-image-data (image <png-compressed-image>) (uncompress? <boolean>))
  "Get the PNG image data as a single bytevector.  When UNCOMPRESS? option is
set to #t, the procedure returns data in uncompressed form."
  (let ((data-chunks (png-image-chunks-query image 'IDAT)))
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

(define-method (png-image-data (image <png-compressed-image>))
  "Get the decompressed PNG image data as a single bytevector."
  (png-image-data image #t))

(define-method (png-image->png (image <png-compressed-image>) (port <output-port>))
  (put-bytevector port %png-image-signature)
  (for-each (lambda (chunk)
              (png-chunk->png chunk port))
            (png-image-chunks image)))



(define-class <png-image> (<png-compressed-image>)
  ;; IDAT: Image data.
  (data
   #:init-thunk   (lambda () (make-bytevector 0))
   #:init-keyword #:data
   #:setter       png-image-data-set!
   #:getter       png-image-data)

  (data-chunk-size
   #:init-value   256
   #:init-keyword #:data-chunk-size
   #:getter       png-image-data-chunk-size))

(define (png-image? x)
  "Check if X is a PNG image instance."
  (is-a? x <png-image>))

(define-method (png-compressed-image-decompress (image <png-compressed-image>))
  "Decompress an IMAGE, return a new <png-image> instance with uncompressed
data."
  (let ((chunks (map (lambda (chunk)
                       (png-chunk->typed-chunk image chunk))
                     (map png-chunk-clone (png-image-chunks image)))))
    (make <png-image>
      #:chunks chunks
      #:header (png-image-chunks-query chunks 'IHDR)
      #:palette (let ((plte-chunks (png-image-chunks-query chunks 'PLTE)))
                  (and (not (null? plte-chunks))
                       (car plte-chunks)))
      #:data (png-image-data image)
      #:data-chunk-size (let ((idat (car (png-image-chunks-query image 'IDAT))))
                          (png-chunk-length idat)))))

(define* (png-image-compress image
                             #:key
                             (data-chunk-size #f))
  (let* ((data            (png-image-data image))
         (compressed-data (compress data))
         (chunk-size      (or data-chunk-size
                              (png-image-data-chunk-size image)))
         (segments        (map (lambda (data)
                                 (let ((chunk (make <png-chunk>
                                                #:length (bytevector-length data)
                                                #:type   'IDAT
                                                #:data   data)))
                                   (png-chunk-crc-update! chunk)
                                   chunk))
                               (bytevector-split data chunk-size)))
         (old-chunks (png-image-chunks-query image
                                             (lambda (chunk)
                                               (and (not (equal? (png-chunk-type chunk)
                                                                 'IDAT))
                                                    (not (equal? (png-chunk-type chunk)
                                                                 'IEND)))))))
    (make <png-compressed-image>
      #:chunks (append old-chunks (append segments (list (make <png-chunk:IEND>)))))))

(define-method (png-image->png (image <png-image>) (port <output-port>))
  (let ((compressed-image (png-image-compress image)))
    (png-image->png compressed-image port)))

;; image.scm ends here.
