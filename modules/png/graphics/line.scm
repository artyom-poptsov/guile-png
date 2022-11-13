(define-module (png graphics line)
  #:use-module (oop goops)
  #:use-module (png image)
  #:use-module (png pixel)
  #:use-module (png graphics graphic)
  #:use-module (png graphics point)
  #:export (<line>
            line-p1
            line-p2))

(define-class <line> (<graphic>)
  (p1
   #:init-keyword #:p1
   #:init-thunk   (lambda () (make <point>))
   #:getter       line-p1)
  (p2
   #:init-keyword #:p2
   #:init-thunk   (lambda () (make <point>))
   #:getter       line-p2))



(define-method (draw! (image <png-image>) (line <line>))
  "This method implements Bresenham's Line generation algorithm."
  (let* ((p1 (line-p1 line))
         (p2 (line-p2 line))
         (x1 (if (< (point-x p1) (point-x p2))
                 (point-x p1)
                 (point-x p2)))
         (y1 (point-y p1))
         (x2 (if (> (point-x p2) (point-x p1))
                 (point-x p2)
                 (point-x p1)))
         (y2 (point-y p2))
         (color (graphic-color line)))
    (if (= x1 x2)
        (let ((delta (if (> y2 y1)
                         1
                         -1)))
          (let loop ((y y1))
            (unless (= y y2)
              (png-image-pixel-set! image x1 y color)
              (loop (+ y delta)))))
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
                          slope-error-new)))))))))

;;; line.scm ends here.
