(define-module (png graphics filled-rectangle)
  #:use-module (oop goops)
  #:use-module (png image)
  #:use-module (png graphics graphic)
  #:use-module (png graphics pixel)
  #:use-module (png graphics point)
  #:use-module (png graphics rectangle)
  #:export (<filled-rectangle>
            filled-rectangle-position
            filled-rectangle-width
            filled-rectangle-height))


(define-class <filled-rectangle> (<rectangle>))

(define filled-rectangle-position rectangle-position)
(define filled-rectangle-height   rectangle-height)
(define filled-rectangle-width    rectangle-width)


(define-method (draw! (image <png-image>) (rectangle <filled-rectangle>))
  (let* ((position   (rectangle-position rectangle))
         (position-x (point-x position))
         (position-y (point-y position))
         (width      (rectangle-width rectangle))
         (height     (rectangle-height rectangle))
         (color      (graphic-color rectangle)))
    (for-each (lambda (y)
                (for-each (lambda (x)
                            (png-image-pixel-set! image
                                                  (+ position-x x)
                                                  (+ position-y y)
                                                  color))
                          (iota width)))
              (iota height))))

;;; filled-rectangle.scm ends here.
