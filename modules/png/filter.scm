(define-module (png filter)
  #:use-module (oop goops)
  #:use-module (rnrs bytevectors)
  #:use-module (png image)
  #:export (png-image-filter-invert-colors))


(define-method (png-image-filter-invert-colors (image <png-image>))
  "Copy IMAGE and its colors.  Return new image."
  (let* ((image-clone (png-image-clone image))
         (data        (png-image-data image-clone))
         (data-length (bytevector-length data))
         (pixel-size  (png-image-pixel-size image)))
    (let loop ((offset 0))
      (if (= offset data-length)
          image-clone
          (begin
            (let loop-over-pixel ((index 0))
              (unless (= index pixel-size)
                (bytevector-u8-set! data
                                    (+ offset index)
                                    (- 255
                                       (bytevector-u8-ref data (+ offset index))))
                (loop-over-pixel (+ index 1))))
            (loop (+ offset pixel-size)))))))

;;; filter.scm ends here.
