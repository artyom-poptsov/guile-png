(define-module (png core chunk chrm)
  #:use-module (srfi srfi-43)
  #:use-module (rnrs bytevectors)
  #:use-module (oop goops)
  #:use-module (png core common)
  #:use-module (png core chunk)
  #:export (<png-chunk:cHRM>
            png-chunk:cHRM-white-point-x
            png-chunk:cHRM-white-point-y
            png-chunk:cHRM-red-x
            png-chunk:cHRM-red-y
            png-chunk:cHRM-green-x
            png-chunk:cHRM-green-y
            png-chunk:cHRM-blue-x
            png-chunk:cHRM-blue-y

            data:white-point-x
            data:white-point-y
            data:red-x
            data:red-y
            data:green-x
            data:green-y
            data:blue-x
            data:blue-y

            png-chunk->png-chunk:cHRM))



(define-class <png-chunk:cHRM> (<png-chunk>)
  (white-point-x
   #:init-keyword #:white-point-x
   #:getter       png-chunk:cHRM-white-point-x)

  (white-point-y
   #:init-keyword #:white-point-y
   #:getter       png-chunk:cHRM-white-point-y)

  (red-x
   #:init-keyword #:red-x
   #:getter       png-chunk:cHRM-red-x)

  (red-y
   #:init-keyword #:red-y
   #:getter       png-chunk:cHRM-red-y)

  (green-x
   #:init-keyword #:green-x
   #:getter       png-chunk:cHRM-green-x)

  (green-y
   #:init-keyword #:green-y
   #:getter       png-chunk:cHRM-green-y)

  (blue-x
   #:init-keyword #:blue-x
   #:getter       png-chunk:cHRM-blue-x)

  (blue-y
   #:init-keyword #:blue-y
   #:getter       png-chunk:cHRM-blue-y))



(define-method (initialize (chunk <png-chunk:cHRM>) initargs)
  (next-method)
  (slot-set! chunk 'type 'cHRM))


(define-method (%display (chunk <png-chunk:cHRM>) (port <port>))
  (let ((type (png-chunk-type-info chunk)))
    (format port "#<png-chunk:cHRM ~a ~a>"
            (list-ref type 2)
            (object-address/hex-string chunk))))

(define-method (display (chunk <png-chunk:cHRM>) (port <port>))
  (%display chunk port))

(define-method (write (chunk <png-chunk:cHRM>) (port <port>))
  (%display chunk port))



(define-method (data:white-point-x (vec <bytevector>))
  (vector->int32 (bytevector-copy/part vec 0 4)))

(define-method (data:white-point-y (vec <bytevector>))
  (vector->int32 (bytevector-copy/part vec 4 4)))

(define-method (data:red-x (vec <bytevector>))
  (vector->int32 (bytevector-copy/part vec 8 4)))

(define-method (data:red-y (vec <bytevector>))
  (vector->int32 (bytevector-copy/part vec 12 4)))

(define-method (data:green-x (vec <bytevector>))
  (vector->int32 (bytevector-copy/part vec 16 4)))

(define-method (data:green-y (vec <bytevector>))
  (vector->int32 (bytevector-copy/part vec 20 4)))

(define-method (data:blue-x (vec <bytevector>))
  (vector->int32 (bytevector-copy/part vec 24 4)))

(define-method (data:blue-y (vec <bytevector>))
  (vector->int32 (bytevector-copy/part vec 28 4)))

(define-method (png-chunk->png-chunk:cHRM (chunk <png-chunk>))
  (let ((length (png-chunk-length chunk))
        (type   (png-chunk-type chunk))
        (data   (png-chunk-data chunk))
        (crc    (png-chunk-crc chunk)))
    (make <png-chunk:cHRM>
      #:length             length
      #:type               type
      #:data               data
      #:crc                crc
      #:white-point-x      (data:white-point-x data)
      #:white-point-y      (data:white-point-y data)
      #:red-x              (data:red-x data)
      #:red-y              (data:red-y data)
      #:green-x            (data:green-x data)
      #:green-y            (data:green-y data)
      #:blue-x             (data:blue-x data)
      #:blue-y             (data:blue-y data))))

(define-method (png-chunk-encode (chunk <png-chunk:cHRM>))
  "Encode a cHRM CHUNK."
  (let* ((white-point-x (png-chunk:cHRM-white-point-x chunk))
         (white-point-y (png-chunk:cHRM-white-point-y chunk))
         (red-x         (png-chunk:cHRM-red-x chunk))
         (red-y         (png-chunk:cHRM-red-y chunk))
         (green-x       (png-chunk:cHRM-green-x chunk))
         (green-y       (png-chunk:cHRM-green-y chunk))
         (blue-x        (png-chunk:cHRM-blue-x chunk))
         (blue-y        (png-chunk:cHRM-blue-y chunk))
         (data          (make-bytevector 32 0))
         (encoded-chunk (make <png-chunk>
                          #:data   data
                          #:length (bytevector-length data)
                          #:type   'cHRM)))
    (bytevector-copy! (int32->bytevector white-point-x) 0 data 0 4)
    (bytevector-copy! (int32->bytevector white-point-y) 0 data 4 4)
    (bytevector-copy! (int32->bytevector red-x)         0 data 8 4)
    (bytevector-copy! (int32->bytevector red-y)         0 data 12 4)
    (bytevector-copy! (int32->bytevector green-x)       0 data 16 4)
    (bytevector-copy! (int32->bytevector green-y)       0 data 20 4)
    (bytevector-copy! (int32->bytevector blue-x)        0 data 24 4)
    (bytevector-copy! (int32->bytevector blue-y)        0 data 28 4)
    (png-chunk-crc-update! encoded-chunk)
    encoded-chunk))

;;; chunk-iend.scm ends here.
