(define-module (png core chunk-ztxt)
  #:use-module (srfi srfi-43)
  #:use-module (oop goops)
  #:use-module (png core common)
  #:use-module (png core chunk)
  #:export (<png-chunk:zTXt>
            png-chunk:zTXt-keyword
            png-chunk:zTXt-compression-method
            png-chunk:zTXt-text
            data->png-chunk:zTXt))



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



(define-method (%display (chunk <png-chunk:zTXt>) (port <port>))
  (let ((type (vector->chunk-type (png-chunk-type chunk))))
    (format port "#<png-chunk:zTXt ~a: ~a ~a>"
            (list-ref type 2)
            (png-chunk:zTXt-keyword chunk)
            (object-address/hex-string chunk))))

(define-method (display (chunk <png-chunk:zTXt>) (port <port>))
  (%display chunk port))

(define-method (write (chunk <png-chunk:zTXt>) (port <port>))
  (%display chunk port))



(define-method (data:keyword (vec <vector>))
  (let loop ((keyword '())
             (index   0))
    (if (zero? (vector-ref vec index))
        (list->string keyword)
        (loop (append keyword (list (integer->char (vector-ref vec index))))
              (+ index 1)))))

(define-method (data->png-chunk:zTXt (data   <vector>)
                                     (type   <vector>)
                                     (length <number>)
                                     (crc    <vector>))
  (define (read-text index output)
    (let loop ((text '())
               (idx  index))
      (if (= idx (vector-length data))
          (make <png-chunk:zTXt>
            #:length  length
            #:type    type
            #:data    data
            #:crc     crc
            #:keyword            (assoc-ref output 'keyword)
            #:compression-method (assoc-ref output 'compression-method)
            ;; TODO: Decompress the datastream.
            #:text               (list->vector text))
          (loop (append text (list (vector-ref data idx)))
                (+ idx 1)))))


  (define (read-compression-method index output)
    (read-text (+ index 1)
               (acons 'compression-method
                      (vector-ref data index)
                      output)))

  (define (read-keyword)
    (let loop ((keyword '())
               (index   0))
      (if (zero? (vector-ref data index))
          (read-compression-method index `((keyword . ,(list->string keyword))))
          (loop (append keyword (list (integer->char (vector-ref data index))))
                (+ index 1)))))

  (read-keyword))
