(define-module (png core chunk zTXt)
  #:use-module (srfi srfi-43)
  #:use-module (rnrs bytevectors)
  #:use-module (zlib)
  #:use-module (oop goops)
  #:use-module (png core common)
  #:use-module (png core chunk)
  #:export (<png-chunk:zTXt>
            png-chunk:zTXt-keyword
            png-chunk:zTXt-compression-method
            png-chunk:zTXt-text
            png-chunk-decode-zTXt))


(define-class <png-chunk:zTXt> (<png-chunk>)
  ;; <string>
  (keyword
   #:init-keyword #:keyword
   #:getter       png-chunk:zTXt-keyword)

  (compression-method
   #:init-keyword #:compression-method
   #:getter       png-chunk:zTXt-compression-method)

  (text
   #:init-keyword #:text
   #:getter       png-chunk:zTXt-text))

(define-method (initialize (chunk <png-chunk:zTXt>) initargs)
  (next-method)
  (slot-set! chunk 'type 'zTXt))



(define-method (%display (chunk <png-chunk:zTXt>) (port <port>))
  (let ((type (png-chunk-type-info chunk)))
    (format port "#<png-chunk:zTXt ~a: ~a ~a>"
            (list-ref type 2)
            (png-chunk:zTXt-keyword chunk)
            (object-address/hex-string chunk))))

(define-method (display (chunk <png-chunk:zTXt>) (port <port>))
  (%display chunk port))

(define-method (write (chunk <png-chunk:zTXt>) (port <port>))
  (%display chunk port))



(define-method (png-chunk-decode-zTXt (chunk <png-chunk>))
  (let ((length (png-chunk-length chunk))
        (type   (png-chunk-type chunk))
        (data   (png-chunk-data chunk))
        (crc    (png-chunk-crc chunk)))

    (define (read-text index keyword compression-method)
      (let* ((compressed-text (bytevector-copy/part data
                                                    index
                                                    (- (bytevector-length data)
                                                       index)))
             (text (uncompress compressed-text)))
        (make <png-chunk:zTXt>
          #:length  length
          #:type    type
          #:data    data
          #:crc     crc
          #:keyword            keyword
          #:compression-method compression-method
          #:text               text)))

    (define (read-compression-method index keyword)
      (read-text (+ index 1)
                 keyword
                 (bytevector-u8-ref data index)))

    (define (read-keyword)
      (let loop ((keyword '())
                 (index   0))
        (if (zero? (bytevector-u8-ref data index))
            (read-compression-method (+ index 1) (list->string keyword))
            (loop (append keyword (list (integer->char (bytevector-u8-ref data index))))
                  (+ index 1)))))

    (read-keyword)))
