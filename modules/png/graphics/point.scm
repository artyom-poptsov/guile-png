(define-module (png graphics point)
  #:use-module (oop goops)
  #:use-module (png image)
  #:use-module (png core common)
  #:use-module (png graphics pixel)
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


(define-method (%display (point <point>) (port <port>))
  (format port "#<point x: ~a y: ~a ~a>"
          (point-x point)
          (point-y point)
          (object-address/hex-string point)))

(define-method (display (point <point>) (port <port>))
  (%display point port))

(define-method (write (point <point>) (port <port>))
  (%display point port))



(define-method (draw! (image <png-image>) (point <point>))
  (png-image-pixel-set! image
                        (point-x point)
                        (point-y point)
                        (graphic-color point)))

;;; point.scm ends here.
