(define-module (png filter)
  #:use-module (oop goops)
  #:use-module (rnrs bytevectors)
  #:use-module (png image)
  #:export (png-image-filter-invert-colors))


(define-method (png-image-filter-invert-colors (image <png-image>))
  "Copy IMAGE and its colors.  Return new image."
  (let* ((image-clone (png-image-clone image))
         (data        (png-image-data image-clone))
         (data-length (bytevector-length data)))
  (let loop ((offset 0))
    (if (= offset data-length)
        image-clone
        (let* ((a (bytevector-u8-ref data offset))
               (r (bytevector-u8-ref data (+ offset 1)))
               (g (bytevector-u8-ref data (+ offset 2)))
               (b (bytevector-u8-ref data (+ offset 3)))
               (new-a a)
               (new-r (- 255 r))
               (new-g (- 255 g))
               (new-b (- 255 b)))
          ;; (format (current-error-port)
          ;;         "a: ~a -> ~a; r: ~a -> ~a; g: ~a -> ~a; b: ~a -> ~a~%"
          ;;         a new-a r new-r g new-g b new-b)
          (bytevector-u8-set! data offset new-a)
          (bytevector-u8-set! data (+ offset 1) new-r)
          (bytevector-u8-set! data (+ offset 2) new-g)
          (bytevector-u8-set! data (+ offset 3) new-b)
          (loop (+ offset 4)))))))

;;; filter.scm ends here.
