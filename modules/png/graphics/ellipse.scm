(define-module (png graphics ellipse)
  #:use-module (oop goops)
  #:use-module (png image)
  #:use-module (png graphics pixel)
  #:use-module (png graphics graphic)
  #:use-module (png graphics point)
  #:export (<ellipse>
            ellipse-center
            ellipse-width
            ellipse-height))


(define-class <ellipse> (<graphic>)
  ;; <point>
  (center
   #:init-keyword #:center
   #:init-thunk   (lambda () (make <point>))
   #:getter       ellipse-center)

  ;; <number>
  (width
   #:init-keyword #:width
   #:init-value   0
   #:getter       ellipse-width)

  ;; <number>
  (height
   #:init-keyword #:height
   #:init-value   0
   #:getter       ellipse-height))



(define-method (draw! (image <png-image>) (ellipse <ellipse>))
  (let* ((center (ellipse-center ellipse))
         (width  (ellipse-width ellipse))
         (height (ellipse-height ellipse))
         (rx     (/ width 2))
         (ry     (/ height 2))
         (color  (graphic-color ellipse)))

    (define* (draw-region-2 #:key x y d2 dx dy)
      (when (>= y 0)
        (png-image-pixel-set! image
                              (+ x (point-x center))
                              (+ y (point-y center))
                              color)
        (png-image-pixel-set! image
                              (+ (- x) (point-x center))
                              (+ y (point-y center))
                              color)
        (png-image-pixel-set! image
                              (+ x (point-x center))
                              (+ (- y) (point-y center))
                              color)
        (png-image-pixel-set! image
                              (+ (- x) (point-x center))
                              (+ (- y) (point-y center))
                              color)
        (if (> d2 0)
            (let ((new-dy (- dy (* 2 rx rx))))
              (draw-region-2 #:x x
                             #:y (- y 1)
                             #:d2 (+ d2 (* rx rx) (- new-dy))
                             #:dy new-dy
                             #:dx dx))
            (let ((new-dy (- dy (* 2 rx rx)))
                  (new-dx (+ dx (* 2 ry ry))))
              (draw-region-2 #:x (+ x 1)
                             #:y (- y 1)
                             #:d2 (+ d2
                                     new-dx
                                     (- new-dy)
                                     (* rx rx))
                             #:dx new-dx
                             #:dy new-dy)))))

    (define* (draw-region-1 #:key x y d1 dx dy)
      (if (< dx dy)
          (begin
            (png-image-pixel-set! image
                                  (+ x (point-x center))
                                  (+ y (point-y center))
                                  color)
            (png-image-pixel-set! image
                                  (+ (- x) (point-x center))
                                  (+ y (point-y center))
                                  color)
            (png-image-pixel-set! image
                                  (+ x (point-x center))
                                  (+ (- y) (point-y center))
                                  color)
            (png-image-pixel-set! image
                                  (+ (- x) (point-x center))
                                  (+ (- y) (point-y center))
                                  color)
            (if (< d1 0)
                (let ((new-dx (+ dx (* 2 ry ry))))
                  (draw-region-1 #:x (+ x 1)
                                 #:y y
                                 #:d1 (+ d1 (+ new-dx (* ry ry)))
                                 #:dx new-dx
                                 #:dy dy))
                (let ((new-dx (+ dx (* 2 ry ry)))
                      (new-dy (- dy (* 2 rx rx))))
                  (draw-region-1 #:x (+ x 1)
                                 #:y (- y 1)
                                 #:d1 (+ d1
                                         new-dx
                                         (- new-dy)
                                         (* ry ry))
                                 #:dx new-dx
                                 #:dy new-dy))))
          (draw-region-2 #:x x
                         #:y y
                         #:d2 (+ (* (* ry ry)
                                    (* (+ x 0.5)
                                       (+ x 0.5)))
                                 (* (* rx rx)
                                    (* (- y 1)
                                       (- y 1)))
                                 (- (* rx rx ry ry)))
                         #:dx dx
                         #:dy dy)))

    (draw-region-1 #:x 0
                   #:y ry
                   #:d1 (- (* ry ry)
                           (+ (* rx rx ry)
                              (* 0.25 rx rx)))
                   #:dx 0
                   #:dy (* 2 rx rx ry))))

