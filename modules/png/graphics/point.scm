(define-module (png graphics point)
  #:use-module (oop goops)
  #:use-module (png image)
  #:use-module (png pixel)
  #:use-module (png graphics graphic)
  #:export (<point>
            point-x
            point-y))

(define-class <point> (<graphic>)
  (x
   #:init-value   0
   #:init-keyword #:x
   #:getter       point-x)
  (y
   #:init-value   0
   #:init-keyword #:y
   #:getter       point-y))



(define-method (draw! (image <png-image>) (point <point>))
  (png-image-pixel-set! image
                        (point-x point)
                        (point-y point)
                        (graphic-color point)))

;;; point.scm ends here.
