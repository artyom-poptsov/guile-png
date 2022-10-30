(define-module (png core chunk-iccp)
  #:use-module (srfi srfi-43)
  #:use-module (rnrs bytevectors)
  #:use-module (oop goops)
  #:use-module (png core common)
  #:use-module (png core chunk)
  #:export (<png-chunk:iCCP>
            png-chunk:iCCP-profile-name
            png-chunk:iCCP-compression-method
            png-chunk:iCCP-profile
            data->png-chunk:iCCP))



(define-class <png-chunk:iCCP> (<png-chunk>)
  ;; <string>
  (profile-name
   #:init-keyword #:profile-name
   #:getter       png-chunk:iCCP-profile-name)

  (compression-method
   #:init-keyword #:compression-method
   #:getter       png-chunk:iCCP-compression-method)

  (profile
   #:init-keyword #:text
   #:getter       png-chunk:iCCP-profile))



(define-method (%display (chunk <png-chunk:iCCP>) (port <port>))
  (let ((type (png-chunk-type-info chunk))))
    (format port "#<png-chunk:iCCP ~a: ~a ~a>"
            (list-ref type 2)
            (png-chunk:iCCP-profile-name chunk)
            (object-address/hex-string chunk))))

(define-method (display (chunk <png-chunk:iCCP>) (port <port>))
  (%display chunk port))

(define-method (write (chunk <png-chunk:iCCP>) (port <port>))
  (%display chunk port))



(define-method (data->png-chunk:iCCP (data   <bytevector>)
                                     (type   <symbol>)
                                     (length <number>)
                                     (crc    <number>))
  (define (read-profile index output)
    (let loop ((profile '())
               (idx  index))
      (if (= idx (bytevector-length data))
          (make <png-chunk:iCCP>
            #:length  length
            #:type    type
            #:data    data
            #:crc     crc
            #:profile-name       (assoc-ref output 'profile-name)
            #:compression-method (assoc-ref output 'compression-method)
            ;; TODO: Decompress the datastream.
            #:profile            (u8-list->bytevector profile))
          (loop (append profile (list (bytevector-u8-ref data idx)))
                (+ idx 1)))))


  (define (read-compression-method index output)
    (read-profile (+ index 1)
                  (acons 'compression-method
                         (bytevector-u8-ref data index)
                         output)))

  (define (read-profile-name)
    (let loop ((profile-name '())
               (index   0))
      (if (zero? (bytevector-u8-ref data index))
          (read-compression-method
           index
           `((profile-name . ,(list->string profile-name))))
          (loop (append profile-name
                        (list (integer->char (bytevector-u8-ref data index))))
                (+ index 1)))))

  (read-profile-name))
