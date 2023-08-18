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



(define-method (%draw-vertical-line! (image <png-image>) (line <line>))
  "Draw a vertical LINE on an IMAGE."
  (let* ((p1 (line-p1 line))
         (p2 (line-p2 line))
         (x1 (point-x p1))
         (y1 (point-y p1))
         (y2 (point-y p2))
         (color (graphic-color line)))
    (if (> y2 y1)
        (let loop ((y y1))
          (unless (> y y2)
            (png-image-pixel-set! image x1 y color)
            (loop (+ y 1))))
        (let loop ((y y1))
          (unless (< y y2)
            (png-image-pixel-set! image x1 y color)
            (loop (- y 1)))))))

(define-method (%draw-horizontal-line! (image <png-image>) (line <line>))
  "Draw a horizontal LINE on an IMAGE."
  (let* ((p1 (line-p1 line))
         (p2 (line-p2 line))
         (x1 (point-x p1))
         (y1 (point-y p1))
         (x2 (point-x p2))
         (color (graphic-color line)))
    (if (> x2 x1)
        (let loop ((x x1))
          (unless (> x x2)
            (png-image-pixel-set! image x y1 color)
            (loop (+ x 1))))
        (let loop ((x x1))
          (unless (< x x2)
            (png-image-pixel-set! image x y1 color)
            (loop (- x 1)))))))

;; Based on the code from
;; <https://www.markwrobel.dk/post/amiga-machine-code-letter12-linedraw/>
(define-method (draw! (image <png-image>) (line <line>))
  "This method implements Bresenham's Line generation algorithm."
  (let* ((p1 (line-p1 line))
         (p2 (line-p2 line))
         (x1 (point-x p1))
         (y1 (point-y p1))
         (x2 (point-x p2))
         (y2 (point-y p2)))
    (cond
     ((equal? x1 x2)
      (%draw-vertical-line! image line))
     ((equal? y1 y2)
      (%draw-horizontal-line! image line))
     (else
      ;; XXX: We need to add 1 to the x2 the algorithm so it will reach the
      ;; final pixel of the line.
      (let* ((x2 (+ (point-x p2) 1))
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
                              (loop (+ fraction (/ 1 a) (- 1))
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
                              (loop (+ fraction (- (- 1) a))
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
                                    (+ y 1))))))))))))))

;;; line.scm ends here.
