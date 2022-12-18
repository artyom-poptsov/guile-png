(define-module (png core chunk tEXt)
  #:use-module (srfi srfi-43)
  #:use-module (rnrs bytevectors)
  #:use-module (ice-9 iconv)
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

(define-method (initialize (chunk <png-chunk:tEXt>) initargs)
  (next-method)
  (slot-set! chunk 'type 'tEXt))

(define (png-chunk:tEXt? x)
  (is-a? x <png-chunk:tEXt>))



(define-method (%display (chunk <png-chunk:tEXt>) (port <port>))
  (let ((type (png-chunk-type-info chunk)))
    (format port "#<png-chunk:tEXt ~a: ~a ~a>"
            (list-ref type 2)
            (png-chunk:tEXt-keyword chunk)
            (object-address/hex-string chunk))))

(define-method (display (chunk <png-chunk:tEXt>) (port <port>))
  (%display chunk port))



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
              (read-text (+ index 1)
                         (utf8->string (u8-list->bytevector (reverse keyword))))
              (loop (cons byte keyword)
                    (+ index 1))))))

    (read-keyword)))

(define-method (png-chunk-encode (chunk <png-chunk:tEXt>))
  (let* ((keyword        (string->bytevector (png-chunk:tEXt-keyword chunk)
                                             "US-ASCII"))
         (keyword-length (bytevector-length keyword))
         (text           (string->bytevector (png-chunk:tEXt-text chunk)
                                             "US-ASCII"))
         (text-length    (bytevector-length text))
         (chunk-length   (+ keyword-length
                            1
                            text-length))
         (d (format (current-error-port) "keyword: ~a; text: ~a~%~%" keyword text))
         (data           (make-bytevector chunk-length 0))
         (encoded-chunk  (make <png-chunk>
                           #:type   'tEXt
                           #:data   data
                           #:length chunk-length)))
    (bytevector-copy! keyword 0 data 0 keyword-length)
    (bytevector-copy! text
                      0
                      data
                      (+ keyword-length 1)
                      text-length)
    (png-chunk-crc-update! encoded-chunk)
    encoded-chunk))


;;; chunk-text.scm ends here.
