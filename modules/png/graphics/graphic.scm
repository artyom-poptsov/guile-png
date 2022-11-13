(define-module (png graphics graphic)
  #:use-module (rnrs bytevectors)
  #:use-module (png image)
  #:use-module (oop goops)
  #:export (<graphic>
            graphic-color
            draw!))

(define-class <graphic> ()
  (color
   #:init-keyword #:color
   #:init-thunk   (lambda () (make-bytevector 4 0))
   #:getter       graphic-color))

(define-method (draw! (image <png-image>) (obj <graphic>))
  (error "Not implemented yet" image obj))

;;; shape.scm ends here.
