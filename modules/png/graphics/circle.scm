(define-module (png graphics circle)
  #:use-module (oop goops)
  #:use-module (png image)
  #:use-module (png graphics pixel)
  #:use-module (png graphics graphic)
  #:use-module (png graphics ellipse)
  #:export (<circle>
            circle-center
            circle-diameter
            circle-radius))


(define-class <circle> (<ellipse>)
  ;; Circle radius.
  ;;
  ;; <number>
  (radius
   #:init-keyword #:radius
   #:init-value   0
   #:getter       circle-radius))


(define-method (initialize (circle <circle>) initargs)
  (next-method)
  (let ((radius   (and (memq #:radius initargs)
                       (cadr (memq #:radius initargs))))
        (diameter (and (memq #:diameter initargs)
                       (cadr (memq #:radius initargs))))
        (height   (and (memq #:height initargs)
                       (cadr (memq #:height initargs))))
        (width    (and (memq #:width initargs)
                       (cadr (memq #:width initargs)))))

    (when (or width height)
      (error "#:width and #:height are unsupported parameters"
             initargs))

    (when (and radius diameter)
      (error "Cannot use both #:radius and #:diameter"
             initargs))

    (when radius
      (let ((size (* radius)))
        (slot-set! circle 'width  size)
        (slot-set! circle 'height size)))

    (when diameter
      (slot-set! circle 'radius (/ diameter 2))
      (slot-set! circle 'width  diameter)
      (slot-set! circle 'height diameter))))


(define circle-center ellipse-center)

(define-method (circle-diameter (circle <circle>))
  (* (circle-radius circle) 2))

;;; circle.scm ends here.
