(define-module (png graphics multiline)
  #:use-module (oop goops)
  #:use-module (png image)
  #:use-module (png core common)
  #:use-module (png graphics graphic)
  #:use-module (png graphics point)
  #:use-module (png graphics line)
  #:export (<multiline>
            multiline-points))


(define-class <multiline> (<graphic>)
  ;; <list> of <point>
  (points
   #:init-keyword #:points
   #:init-value   '()
   #:getter       multiline-points))


(define-method (initialize (multiline <multiline>) initargs)
  (next-method)
  (let ((points (and (memq #:points initargs)
                     (cadr (memq #:points initargs)))))
    (when (or (not points) (< (length points) 2))
      (error "Multiline must have at least two points" initargs))
    (slot-set! multiline 'points points)))


;; Class printers.

(define-method (%display (multiline <multiline>) (port <port>))
  (format port "#<multiline points: ~a ~a>"
          (length (multiline-points multiline))
          (object-address/hex-string multiline)))

(define-method (display (multiline <multiline>) (port <port>))
  (%display multiline port))

(define-method (write (multiline <multiline>) (port <port>))
  (%display multiline port))



(define-method (draw! (image <png-image>) (multiline <multiline>))
  (let ((color (graphic-color multiline)))
    (let loop ((points (multiline-points multiline)))
      (unless (< (length points) 2)
        (let ((line (make <line>
                      #:p1 (car points)
                      #:p2 (cadr points)
                      #:color color)))
          (draw! image line))
        (loop (cdr points))))))

;;; multiline.scm ends here.
