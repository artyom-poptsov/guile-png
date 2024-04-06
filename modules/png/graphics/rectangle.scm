(define-module (png graphics rectangle)
  #:use-module (oop goops)
  #:use-module (png image)
  #:use-module (png core common)
  #:use-module (png graphics graphic)
  #:use-module (png graphics point)
  #:use-module (png graphics line)
  #:export (<rectangle>
            rectangle-position
            rectangle-width
            rectangle-height

            <square>
            square-size))


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

(define-class <square> (<rectangle>))

(define-method (square-size (square <square>))
  (rectangle-width square))

(define-method (initialize (square <square>) initargs)
  (next-method)
  (let ((size (and (memq #:size initargs)
                   (cadr (memq #:size initargs)))))

    (unless size
      (error "Square size must be specified" initargs))

    (slot-set! square 'width size)
    (slot-set! square 'height size)))



(define-method (%display (rectangle <rectangle>) (port <port>))
  (format port "#<rectangle position: ~a width: ~a height: ~a ~a>"
          (rectangle-position rectangle)
          (rectangle-width rectangle)
          (rectangle-height rectangle)
          (object-address/hex-string rectangle)))

(define-method (display (rectangle <rectangle>) (port <port>))
  (%display rectangle port))

(define-method (write (rectangle <rectangle>) (port <port>))
  (%display rectangle port))

(define-method (%display (square <square>) (port <port>))
  (format port "#<square position: ~a size: ~a ~a>"
          (rectangle-position square)
          (square-size square)
          (object-address/hex-string square)))

(define-method (display (square <square>) (port <port>))
  (%display square port))

(define-method (write (square <square>) (port <port>))
  (%display square port))


(define-method (draw! (image <png-image>) (rectangle <rectangle>))
  (let* ((position (rectangle-position rectangle))
         (width    (rectangle-width rectangle))
         (height   (rectangle-height rectangle))
         (color    (graphic-color rectangle))
         (lines    (list
                    (make <line>
                      ;; Upper left.
                      #:p1 (make <point>
                             #:x (point-x position)
                             #:y (point-y position))
                      ;; Upper right.
                      #:p2 (make <point>
                             #:x (+ (point-x position) (- width 1))
                             #:y (point-y position))
                      #:color color)
                    (make <line>
                      ;; Upper right.
                      #:p1 (make <point>
                             #:x (+ (point-x position) (- width 1))
                             #:y (point-y position))
                      ;; Bottom right.
                      #:p2 (make <point>
                             #:x (+ (point-x position) (- width 1))
                             #:y (+ (point-y position) (- height 1)))
                      #:color color)
                    (make <line>
                      ;; Bottom right.
                      #:p1 (make <point>
                             #:x (+ (point-x position) (- width 1))
                             #:y (+ (point-y position) (- height 1)))
                      ;; Bottom left.
                      #:p2 (make <point>
                             #:x (point-x position)
                             #:y (+ (point-y position) (- height 1)))
                      #:color color)
                    (make <line>
                      ;; Bottom left.
                      #:p1 (make <point>
                             #:x (point-x position)
                             #:y (+ (point-y position) (- height 1)))
                      ;; Upper left.
                      #:p2 (make <point>
                             #:x (point-x position)
                             #:y (point-y position))
                      #:color color))))
    (for-each (lambda (line)
                (draw! image line))
              lines)))

;;; rectangle.scm ends here.
