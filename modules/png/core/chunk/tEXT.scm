(define-module (png core chunk tEXT)
  #:use-module (srfi srfi-43)
  #:use-module (rnrs bytevectors)
  #:use-module (oop goops)
  #:use-module (png core common)
  #:use-module (png core chunk)
  #:use-module (png core chunk tEXt)
  #:export (<png-chunk:tEXT>
            png-chunk:tEXT?
            png-chunk:tEXT-keyword
            png-chunk:tEXT-text
            png-chunk-decode-tEXT))



(define-class <png-chunk:tEXT> (<png-chunk:tEXt>))

(define (png-chunk:tEXT? x)
  (is-a? x <png-chunk:tEXT>))

(define png-chunk:tEXT-keyword png-chunk:tEXt-keyword)
(define png-chunk:tEXT-text    png-chunk:tEXt-text)



(define-method (%display (chunk <png-chunk:tEXT>) (port <port>))
  (let ((type (png-chunk-type-info chunk)))
    (format port "#<png-chunk:tEXT ~a: ~a ~a>"
            (list-ref type 2)
            (png-chunk:tEXT-keyword chunk)
            (object-address/hex-string chunk))))

(define-method (display (chunk <png-chunk:tEXT>) (port <port>))
  (%display chunk port))



(define-method (png-chunk-decode-tEXT (chunk <png-chunk>))
  (let ((length (png-chunk-length chunk))
        (type   (png-chunk-type chunk))
        (data   (png-chunk-data chunk))
        (crc    (png-chunk-crc chunk)))
    (define (read-text index keyword)
      (let loop ((text '())
                 (idx  index))
        (if (= idx (bytevector-length data))
            (make <png-chunk:tEXT>
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
              (read-text (+ index 1)
                         (utf8->string (u8-list->bytevector (reverse keyword))))
              (loop (cons byte keyword)
                    (+ index 1))))))

    (read-keyword)))

;;; chunk-text.scm ends here.
