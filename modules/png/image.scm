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
               (result (make-bytevector 0)))
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



(define-method (%display (image <png-image>) (port <port>))
  (let ((ihdr (png-image-header image)))
    (format port "#<png-image ~ax~a ~a bit ~a>"
            (png-chunk:IHDR-width ihdr)
            (png-chunk:IHDR-height ihdr)
            (png-chunk:IHDR-bit-depth ihdr)
            (object-address/hex-string image))))



;; 4.1.3. IDAT Image data
;;
;; The IDAT chunk contains the actual image data.  To create this
;; data:
;;
;; * Begin with image scanlines represented as described in Image layout
;;   (Section 2.3); the layout and total size of this raw data are determined by
;;   the fields of IHDR.
;;
;; * Filter the image data according to the filtering method specified by the
;;   IHDR chunk.  (Note that with filter method 0, the only one currently
;;   defined, this implies prepending a filter type byte to each scanline.)
;;
;; * Compress the filtered data using the compression method
;;   specified by the IHDR chunk.
;;
;; <https://www.rfc-editor.org/rfc/rfc2083#page-18>
(define-method (png-image-data/cleanup-scanlines (ihdr <png-chunk:IHDR>)
                                                 (image-data <bytevector>))
  "This method removes filter data from each scanline of IMAGE-DATA.  Return a
new bytevector with image data with filter type bytes removed."
  (let* ((width             (png-chunk:IHDR-width ihdr))
         (height            (png-chunk:IHDR-height ihdr))
         (image-data-length (bytevector-length image-data))
         (scanline-length   (+ (* width 3) 1))  ; TODO: Handle other color types
         (result            (make-bytevector (* width height 3) 0)))
    (let loop ((result-index 0)
               (source-index 0))
      (if (= source-index image-data-length)
          result
          (if (zero? (euclidean-remainder source-index scanline-length))
              (loop result-index (+ source-index 1))
              (begin
                (bytevector-u8-set! result
                                    result-index
                                    (bytevector-u8-ref image-data source-index))
                (loop (+ result-index 1) (+ source-index 1))))))))

(define-method (png-compressed-image-decompress (image <png-compressed-image>))
  "Decompress an IMAGE, return a new <png-image> instance with uncompressed
data."
  (let ((chunks (map (lambda (chunk)
                       (png-chunk->typed-chunk image chunk))
                     (map png-chunk-clone (png-image-chunks image)))))
    (make <png-image>
      #:chunks chunks
      #:header (car (png-image-chunks-query chunks 'IHDR))
      #:palette (let ((plte-chunks (png-image-chunks-query chunks 'PLTE)))
                  (and (not (null? plte-chunks))
                       (car plte-chunks)))
      #:data (let ((data (png-image-data image)))
               (png-image-data/cleanup-scanlines (car (png-image-chunks-query chunks
                                                                              'IHDR))
                                                 data))
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
                               (bytevector-split compressed-data chunk-size)))
         (old-chunks (png-image-chunks-query image
                                             (lambda (chunk)
                                               (and (not (equal? (png-chunk-type chunk)
                                                                 'IDAT))
                                                    (not (equal? (png-chunk-type chunk)
                                                                 'IEND))))))
         (iend-chunk (let ((ch (make <png-chunk:IEND>)))
                       (png-chunk-crc-update! ch)
                       ch)))
    (make <png-compressed-image>
      #:chunks (append old-chunks (append segments (list iend-chunk))))))

(define-method (png-image->png (image <png-image>) (port <output-port>))
  (let ((compressed-image (png-image-compress image)))
    (png-image->png compressed-image port)))

(define-method (png-image-clone (image <png-image>))
  "Copy a PNG IMAGE, return a new copy."
  (let ((chunks (map png-chunk-clone (png-image-chunks image))))
    (make <png-image>
      #:chunks chunks
      #:header (car (png-image-chunks-query chunks 'IHDR))
      #:palette (let ((plte-chunks (png-image-chunks-query chunks 'PLTE)))
                  (and (not (null? plte-chunks))
                       (car plte-chunks)))
      #:data   (bytevector-copy (png-image-data image)))))


;; image.scm ends here.
