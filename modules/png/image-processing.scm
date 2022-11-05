(define-module (png image-processing)
  #:use-module (oop goops)
  #:use-module (rnrs bytevectors)
  #:use-module (png image)
  #:use-module (png pixel)
  #:export (png-image-filter-invert-colors
            png-image-filter-solarize))


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

(define-method (png-image-filter-solarize (image     <png-image>)
                                          (threshold <number>))
  (let ((image-clone (png-image-clone image))
        (pixel-count (png-image-pixels image)))
    (let loop ((index 0))
      (if (= index pixel-count)
          image-clone
          (let* ((pixel (png-image-pixel-ref image index))
                 (red   (bytevector-u8-ref pixel 0))
                 (green (bytevector-u8-ref pixel 1))
                 (blue  (bytevector-u8-ref pixel 2)))
            (when (< red threshold)
              (bytevector-u8-set! pixel 0 (- 255 red)))
            (when (< green threshold)
              (bytevector-u8-set! pixel 1 (- 255 green)))
            (when (< blue threshold)
              (bytevector-u8-set! pixel 2 (- 255 blue)))
            (png-image-pixel-set! image-clone index pixel)
            (loop (+ index 1)))))))

;;; filter.scm ends here.
