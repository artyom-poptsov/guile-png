(define-module (png graphics dimension)
  #:use-module (oop goops)
  #:use-module (png core common)
  #:export (<dimension>
            dimension?
            dimension-width
            dimension-height))



(define-class <dimension> ()
  ;; <number>
  (width
   #:init-value   0
   #:init-keyword #:width
   #:getter       dimension-width)

  ;; <number>
  (height
   #:init-value   0
   #:init-keyword #:height
   #:getter       dimension-height))


(define-method (%display (dimension <dimension>) (port <port>))
  (format port "#<dimension ~ax~a ~a>"
          (dimension-width dimension)
          (dimension-height dimension)
          (object-address/hex-string dimension)))

(define-method (display (dimension <dimension>) (port <port>))
  (%display dimension port))

(define-method (write (dimension <dimension>) (port <port>))
  (%display dimension port))



(define (dimension? x)
  "Check if X is a <dimension> instance."
  (is-a? x <dimension>))

;;; dimension.scm ends here.
