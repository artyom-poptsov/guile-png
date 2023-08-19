(define-module (png graphics selection)
  #:use-module (oop goops)
  #:use-module (png core common)
  #:use-module (png graphics point)
  #:use-module (png graphics dimension)
  #:export (<selection>
            selection?
            selection-image
            selection-position
            selection-dimension))



(define-class <selection> ()
  ;; <png-image>
  (image
   #:init-value   #f
   #:init-keyword #:image
   #:getter       selection-image)

  ;; <point>
  (position
   #:init-thunk   (lambda () (make <point>))
   #:init-keyword #:position
   #:getter       selection-position)

  ;; <dimension>
  (dimension
   #:init-thunk   (lambda () (make <dimension>))
   #:init-keyword #:dimension
   #:getter       selection-dimension))



(define (selection? x)
  "Check if X is a <selection> instance."
  (is-a? x <selection>))


(define-method (%display (selection <selection>) (port <port>))
  (format port "#<selection image: ~a position: ~a dimension: ~a ~a>"
          (selection-image selection)
          (selection-position selection)
          (selection-dimension selection)
          (object-address/hex-string selection)))

(define-method (display (selection <selection>) (port <port>))
  (%display selection port))

(define-method (write (selection <selection>) (port <port>))
  (%display selection port))

;;; selection.scm ends here.
