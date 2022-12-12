(define-module (png graphics line)
  #:use-module (oop goops)
  #:use-module (png image)
  #:use-module (png graphics pixel)
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



;; Based on the code from
;; <https://www.markwrobel.dk/post/amiga-machine-code-letter12-linedraw/>
(define-method (draw! (image <png-image>) (line <line>))
  "This method implements Bresenham's Line generation algorithm."
  (let* ((p1 (line-p1 line))
         (p2 (line-p2 line))
         (x1 (point-x p1))
         (y1 (point-y p1))
         ;; XXX: We need to add 1 to the x2 the algorithm so it will reach the
         ;; final pixel of the line.
         (x2 (+ (point-x p2) 1))
         (y2 (point-y p2))
         (dx (- x2 x1))
         (dy (- y2 y1))
         (a  (/ dy dx))
         (color (graphic-color line)))
    (png-image-pixel-set! image x1 y1 color)
    (if (<= (abs dy) (abs dx))
        (if (< dx 0)
            (if (< dy 0)
                ;; Octant 3
                (let loop ((fraction (+ 0.5 a))
                           (x x1)
                           (y y1))
                  (when (> x x2)
                    (if (< fraction 1)
                        (begin
                          (png-image-pixel-set! image x y color)
                          (loop (+ fraction a)
                                (- x 1)
                                y))
                        (begin
                          (png-image-pixel-set! image x y color)
                          (loop (+ fraction a (- 1))
                                (- x 1)
                                (- y 1))))))
                ;; Octant 4
                (let loop ((fraction (+ 0.5 (- a)))
                           (x x1)
                           (y y1))
                  (when (> x x2)
                    (if (< fraction 1)
                        (begin
                          (png-image-pixel-set! image x y color)
                          (loop (+ fraction (- a))
                                (- x 1)
                                y))
                        (begin
                          (png-image-pixel-set! image x y color)
                          (loop (+ fraction (- a) (- 1))
                                (- x 1)
                                (+ y 1)))))))
            (if (< dy 0)
                ;; Octant 0
                (let loop ((fraction (+ 0.5 (- a)))
                           (x x1)
                           (y y1))
                  (when (< x x2)
                    (if (< fraction 1)
                        (begin
                          (png-image-pixel-set! image x y color)
                          (loop (+ fraction (- a))
                                (+ x 1)
                                y))
                        (begin
                          (png-image-pixel-set! image x y color)
                          (loop (+ fraction (- a) (- 1))
                                (+ x 1)
                                (- y 1))))))
                ;; Octant 7
                (let loop ((fraction (+ 0.5 a))
                           (x x1)
                           (y y1))
                  (when (< x x2)
                    (if (< fraction 1)
                        (begin
                          (png-image-pixel-set! image x y color)
                          (loop (+ fraction a)
                                (+ x 1)
                                y))
                        (begin
                          (png-image-pixel-set! image x y color)
                          (loop (+ fraction a (- 1))
                                (+ x 1)
                                (+ y 1))))))))
        ;; (> (abs dy) (abs dx))
        (if (< dx 0)
            (if (< dy 0)
                ;; Octant 2
                (let loop ((fraction (+ 0.5 (/ 1 a)))
                           (x x1)
                           (y y1))
                  (when (> y y2)
                    (if (< fraction 1)
                        (begin
                          (png-image-pixel-set! image x y color)
                          (loop (+ fraction (/ 1 a))
                                x
                                (- y 1)))
                        (begin
                          (png-image-pixel-set! image x y color)
                          (loop (+ fraction a (/ 1 a) (- 1))
                                (- x 1)
                                (- y 1))))))
                ;; Octant 5
                (let loop ((fraction (+ 0.5 (/ (- 1) a)))
                           (x x1)
                           (y y1))
                  (when (< y y2)
                    (if (< fraction 1)
                        (begin
                          (png-image-pixel-set! image x y color)
                          (loop (+ fraction (- (-1) a))
                                x
                                (+ y 1)))
                        (begin
                          (png-image-pixel-set! image x y color)
                          (loop (+ fraction (/ (- 1) a) (- 1))
                                (- x 1)
                                (+ y 1)))))))
            (if (< dy 0)
                ;; Octant 1
                (let loop ((fraction (+ 0.5 (/ (- 1) a)))
                           (x x1)
                           (y y1))
                  (when (> y y2)
                    (if (< fraction 1)
                        (begin
                          (png-image-pixel-set! image x y color)
                          (loop (+ fraction (/ (- 1) a))
                                x
                                (- y 1)))
                        (begin
                          (png-image-pixel-set! image x y color)
                          (loop (+ fraction (/ (- 1) a) (- 1))
                                (+ x 1)
                                (- y 1))))))
                ;; Octant 6
                (let loop ((fraction (+ 0.5 (/ 1 a)))
                           (x x1)
                           (y y1))
                  (when (< y y2)
                    (if (< fraction 1)
                        (begin
                          (png-image-pixel-set! image x y color)
                          (loop (+ fraction (/ 1 a))
                                x
                                (+ y 1)))
                        (begin
                          (png-image-pixel-set! image x y color)
                          (loop (+ fraction (/ 1 a) (- 1))
                                (+ x 1)
                                (+ y 1)))))))))))

;;; line.scm ends here.
