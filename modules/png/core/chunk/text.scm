(define-module (png core chunk text)
  #:use-module (srfi srfi-43)
  #:use-module (rnrs bytevectors)
  #:use-module (oop goops)
  #:use-module (png core common)
  #:use-module (png core chunk)
  #:export (<png-chunk:tEXt>
            png-chunk:tEXt?
            png-chunk:tEXt-keyword
            png-chunk:tEXt-text
            png-chunk-decode-tEXt))



(define-class <png-chunk:tEXt> (<png-chunk>)
  ;; <string>
  (keyword
   #:init-value   ""
   #:init-keyword #:keyword
   #:getter       png-chunk:tEXt-keyword)

  ;; <string>
  (text
   #:init-value   ""
   #:init-keyword #:text
   #:getter       png-chunk:tEXt-text))

(define (png-chunk:tEXt? x)
  (is-a? x <png-chunk:tEXt>))



(define-method (%display (chunk <png-chunk:tEXt>) (port <port>))
  (let ((type (png-chunk-type-info chunk)))
    (format port "#<png-chunk:tEXt ~a: ~a ~a>"
            (list-ref type 2)
            (png-chunk:tEXt-keyword chunk)
            (object-address/hex-string chunk))))



(define-method (png-chunk-decode-tEXt (chunk <png-chunk>))
  (let ((length (png-chunk-length chunk))
        (type   (png-chunk-type chunk))
        (data   (png-chunk-data chunk))
        (crc    (png-chunk-crc chunk)))
    (define (read-text index keyword)
      (let loop ((text '())
                 (idx  index))
        (if (= idx (bytevector-length data))
            (make <png-chunk:tEXt>
              #:length  length
              #:type    type
              #:data    data
              #:crc     crc
              #:keyword keyword
              #:text    (utf8->string (u8-list->bytevector (reverse text))))
            (loop (cons (bytevector-u8-ref data idx) text)
                  (+ idx 1)))))

    (define (read-keyword)
      (let loop ((keyword '())
                 (index   0))
        (let ((byte (bytevector-u8-ref data index)))
          (if (zero? byte)
              (read-text index (utf8->string (u8-list->bytevector (reverse keyword))))
              (loop (cons byte keyword)
                    (+ index 1))))))

    (read-keyword)))

;;; chunk-text.scm ends here.
