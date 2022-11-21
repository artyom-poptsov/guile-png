(define-module (png core chunk iend)
  #:use-module (oop goops)
  #:use-module (rnrs bytevectors)
  #:use-module (png core common)
  #:use-module (png core chunk)
  #:export (<png-chunk:IEND>
            data->png-chunk:IEND))



(define-class <png-chunk:IEND> (<png-chunk>))

(define-method (initialize (chunk <png-chunk:IEND>) initargs)
  (next-method)
  (slot-set! chunk 'type 'IEND))

(define-method (%display (chunk <png-chunk:IEND>) (port <port>))
  (let ((type (png-chunk-type-info chunk)))
    (format port "#<png-chunk:IEND ~a ~a>"
            (list-ref type 2)
            (object-address/hex-string chunk))))

(define-method (display (chunk <png-chunk:IEND>) (port <port>))
  (%display chunk port))

(define-method (write (chunk <png-chunk:IEND>) (port <port>))
  (%display chunk port))



(define-method (data->png-chunk:IEND (data   <bytevector>)
                                     (type   <symbol>)
                                     (length <number>)
                                     (crc    <number>))
  (make <png-chunk:IEND>
    #:length             length
    #:type               type))

;;; chunk-iend.scm ends here.
