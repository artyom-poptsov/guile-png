(define-module (png graphics rectangle)
  #:use-module (oop goops)
  #:use-module (png image)
  #:use-module (png graphics graphic)
  #:use-module (png graphics point)
  #:use-module (png graphics line)
  #:export (<rectangle>
            rectangle-position
            rectangle-width
            rectangle-height))


(define-class <rectangle> (<graphic>)
  ;; The position of the rectangle.
  ;;
  ;; <point>
  (position
   #:init-keyword #:position
   #:init-thunk   (lambda () (make <point>))
   #:getter       rectangle-position)

  ;; The width of the rectangle.
  ;;
  ;; <number>
  (width
   #:init-keyword #:width
   #:init-value   0
   #:getter       rectangle-width)

  ;; The height of the rectangle.
  ;;
  ;; <number>
  (height
   #:init-keyword #:height
   #:init-value   0
   #:getter       rectangle-height))


(define-method (draw! (image <png-image>) (rectangle <rectangle>))
  (let* ((position (rectangle-position rectangle))
         (width    (rectangle-width rectangle))
         (height   (rectangle-height rectangle))
         (color    (graphic-color rectangle))
         (lines    (list
                    (make <line>
                      #:p1 (make <point>
                             #:x (point-x position)
                             #:y (point-y position))
                      #:p2 (make <point>
                             #:x (+ (point-x position) width)
                             #:y (point-y position))
                      #:color color)
                    (make <line>
                      #:p1 (make <point>
                             #:x (+ (point-x position) width)
                             #:y (point-y position))
                      #:p2 (make <point>
                             #:x (+ (point-x position) width)
                             #:y (+ (point-y position) height))
                      #:color color)
                    (make <line>
                      #:p1 (make <point>
                             #:x (+ (point-x position) width)
                             #:y (+ (point-y position) height))
                      #:p2 (make <point>
                             #:x (point-x position)
                             #:y (+ (point-y position) height))
                      #:color color)
                    (make <line>
                      #:p1 (make <point>
                             #:x (point-x position)
                             #:y (+ (point-y position) height))
                      #:p2 (make <point>
                             #:x (point-x position)
                             #:y (point-y position))
                      #:color color))))
    (for-each (lambda (line)
                (draw! image line))
              lines)))

;;; rectangle.scm ends here.
