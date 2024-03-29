(define-module (png graphics polygon)
  #:use-module (oop goops)
  #:use-module (png image)
  #:use-module (png core common)
  #:use-module (png graphics graphic)
  #:use-module (png graphics line)
  #:use-module (png graphics multiline)
  #:export (<polygon>
            polygon-points))


(define-class <polygon> (<multiline>))

(define polygon-points multiline-points)



(define-method (%display (polygon <polygon>) (port <port>))
  (format port "#<polygon points: ~a ~a>"
          (length (polygon-points polygon))
          (object-address/hex-string polygon)))

(define-method (display (polygon <polygon>) (port <port>))
  (%display polygon port))

(define-method (write (polygon <polygon>) (port <port>))
  (%display polygon port))



(define-method (draw! (image <png-image>) (polygon <polygon>))
  (let ((color (graphic-color polygon)))
    (let loop ((points (polygon-points polygon)))
      (unless (< (length points) 2)
        (let ((line (make <line>
                      #:p1 (car points)
                      #:p2 (cadr points)
                      #:color color)))
          (draw! image line))
        (loop (cdr points))))
    (let ((points (multiline-points polygon)))
      (draw! image (make <line>
                     #:p1 (car points)
                     #:p2 (car (reverse points))
                     #:color color)))))

;;; polygon.scm ends here.
