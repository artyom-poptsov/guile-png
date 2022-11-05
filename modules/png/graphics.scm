(define-module (png graphics)
  #:use-module (rnrs bytevectors)
  #:use-module (oop goops)
  #:use-module (png image)
  #:use-module (png pixel)
  #:export (png-graphics-draw-line!))



(define-method (png-graphics-draw-line! (image <png-image>)
                                        (x1 <number>)
                                        (y1 <number>)
                                        (x2 <number>)
                                        (y2 <number>)
                                        (color <bytevector>))
  "This method implements Bresenham's Line generation algorithm."
  (let ((m-new (* 2 (- y2 y1))))
    (let loop ((x x1)
               (y y1)
               (slope-error (- m-new (- x2 x1))))
      (when (<= x x2)
        (png-image-pixel-set! image x y color)
        (let ((slope-error-new (+ slope-error m-new)))
          (if (>= slope-error-new 0)
              (loop (+ x 1)
                    (+ y 1)
                    (- slope-error-new (* 2 (- x2 x1))))
              (loop (+ x 1)
                    y
                    slope-error-new)))))))

;;; graphics.scm ends here.
