(define-module (png image)
  #:use-module (oop goops)
  #:use-module (ice-9 binary-ports)     ; put-bytevector
  #:use-module (png core common)
  #:use-module (png core chunk)
  #:use-module (png core chunk-ihdr)
  #:export (<png-image>
            png-image?
            png-image-chunks
            png-image-chunks-query
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
  #(137 80 78 71 13 10 26 10))


(define-class <png-image> ()
  ;; <list> of <png-chunk>
  (chunks
   #:init-value   '()
   #:init-keyword #:chunks
   #:getter       png-image-chunks))

(define (png-image? x)
  "Check if X is a PNG image instance."
  (is-a? x <png-image>))



(define-method (%display (image <png-image>) (port <port>))
  (let ((ihdr (car (png-image-chunks-query image 'IHDR))))
    (format port "#<png-image ~ax~a ~a bit ~a>"
            (png-chunk:IHDR-width ihdr)
            (png-chunk:IHDR-height ihdr)
            (png-chunk:IHDR-bit-depth ihdr)
            (object-address/hex-string image))))

(define-method (display (image <png-image>) (port <port>))
  (%display image port))

(define-method (write (image <png-image>) (port <port>))
  (%display image port))


(define-method (png-image-chunks-query (image <png-image>) (predicate <procedure>))
  (filter predicate (png-image-chunks image)))

(define-method (png-image-chunks-query (image <png-image>) (chunk <symbol>))
  (png-image-chunks-query image (lambda (c)
                                  (equal? (png-chunk-type/name c) chunk))))

(define-method (png-image-chunks-query (image <png-image>) (chunk <vector>))
  (png-image-chunks-query image (lambda (c)
                                  (equal? (png-chunk-type c) chunk))))



(define-method (png-image-width (image <png-image>))
  (let ((ihdr (car (png-image-chunks-query image 'IHDR))))
    (png-chunk:IHDR-width ihdr)))

(define-method (png-image-height (image <png-image>))
  (let ((ihdr (car (png-image-chunks-query image 'IHDR))))
    (png-chunk:IHDR-height ihdr)))

(define-method (png-image-bit-depth (image <png-image>))
  (let ((ihdr (car (png-image-chunks-query image 'IHDR))))
    (png-chunk:IHDR-bit-depth ihdr)))

(define-method (png-image-color-type (image <png-image>))
  (let ((ihdr (car (png-image-chunks-query image 'IHDR))))
    (png-chunk:IHDR-color-type ihdr)))



(define-method (png-image-data (image <png-image>))
  "Get the PNG image data as a single vector."
  (let ((data-chunks (png-image-chunks-query image 'IDAT)))
    (let loop ((chunks data-chunks)
               (result (if (null? data-chunks)
                           (make-vector 0)
                           (png-chunk-data (car data-chunks)))))
      (if (null? chunks)
          result
          (let* ((chunk         (car chunks))
                 (chunk-data    (png-chunk-data chunk))
                 (result-length (vector-length result))
                 (chunk-length  (vector-length chunk-data))
                 (new-result    (make-vector (+ result-length chunk-length))))
            (vector-copy! new-result 0 result)
            (vector-copy! new-result result-length chunk-data)
            (loop (cdr chunks) new-result))))))

(define-method (png-image->png (image <png-image>) (port <output-port>))
  (put-bytevector port %png-image-signature)
  (for-each (lambda (chunk)
              (put-bytevector port chunk))
            (png-image-chunks)))

;; image.scm ends here.
