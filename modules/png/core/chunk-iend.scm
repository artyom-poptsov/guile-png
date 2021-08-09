(define-module (png core chunk-iend)
  #:use-module (oop goops)
  #:use-module (png core common)
  #:use-module (png core chunk)
  #:export (<png-chunk:IEND>))



(define-class <png-chunk:IEND> (<png-chunk>))

(define-method (%display (chunk <png-chunk:IEND>) (port <port>))
  (let ((type (vector->chunk-type (png-chunk-type chunk))))
    (format port "#<png-chunk:IEND ~a ~a>"
            (list-ref type 2)
            (object-address/hex-string chunk))))

(define-method (display (chunk <png-chunk:IEND>) (port <port>))
  (%display chunk port))

(define-method (write (chunk <png-chunk:IEND>) (port <port>))
  (%display chunk port))

;;; chunk-iend.scm ends here.
