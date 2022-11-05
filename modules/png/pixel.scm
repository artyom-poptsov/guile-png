(define-module (png pixel)
  #:use-module (oop goops)
  #:use-module (rnrs bytevectors)
  #:use-module (png image)
  #:export (png-image-pixel-ref
            png-image-pixel-set!))



(define-method (png-image-pixel-ref (image <png-image>)
                                    (x     <number>))
  (let* ((data        (png-image-data image))
         (data-length (bytevector-length data))
         (pixel-count (* (png-image-width image) (png-image-height image)))
         (bpp         (png-image-pixel-size image))
         (offset      (* x bpp))
         (result      (make-bytevector bpp 0)))
    (when (>= x pixel-count)
      (error "Pixel reference is outside the data" x pixel-count))
    (let loop ((index 0))
      (if (= index bpp)
          result
          (begin
            (bytevector-u8-set! result
                                index
                                (bytevector-u8-ref data (+ offset index)))
            (loop (+ index 1)))))))

(define-method (png-image-pixel-set! (image <png-image>)
                                     (x     <number>)
                                     (pixel <bytevector>))
  (let* ((data        (png-image-data image))
         (data-length (bytevector-length data))
         (pixel-count (* (png-image-width image) (png-image-height image)))
         (bpp         (png-image-pixel-size image))
         (offset      (* x bpp)))
    (when (>= x pixel-count)
      (error "Pixel reference is outside the data" x pixel-count))
    (let loop ((index 0))
      (unless (= index bpp)
        (bytevector-u8-set! data
                            (+ offset index)
                            (bytevector-u8-ref pixel index))
        (loop (+ index 1))))))

;;; pixel.scm ends here.
