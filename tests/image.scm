(use-modules (srfi srfi-64)
             (srfi srfi-26)
             (oop goops)
             (png image)
             (png core chunk)
             (png core chunk-ihdr)
             (png core chunk-iend))


(define %test-name "image")


(test-begin %test-name)

(test-assert "png-image?"
  (let* ((ihdr  (make <png-chunk:IHDR>
                  #:type      (chunk-type->vector 'IHDR)))
         (iend  (make <png-chunk:IEND>))
         (image (make <png-image>
                  #:chunks    (list ihdr iend))))
    (png-image? image)))

(test-assert "png-image-chunks-query: IHDR"
  (let* ((ihdr  (make <png-chunk:IHDR>
                  #:type      (chunk-type->vector 'IHDR)))
         (iend  (make <png-chunk:IEND>))
         (image (make <png-image>
                  #:chunks    (list ihdr iend)))
         (chunks (png-image-chunks-query image 'IHDR)))
    (and (= (length chunks) 1)
         (car chunks))))

(test-equal "png-image-width"
  200
  (let* ((ihdr  (make <png-chunk:IHDR>
                  #:width     200
                  #:height    100
                  #:type      (chunk-type->vector 'IHDR)))
         (iend  (make <png-chunk:IEND>))
         (image (make <png-image>
                  #:chunks    (list ihdr iend))))
    (png-image-width image)))

(test-equal "png-image-height"
  100
  (let* ((ihdr  (make <png-chunk:IHDR>
                  #:width     200
                  #:height    100
                  #:type      (chunk-type->vector 'IHDR)))
         (iend  (make <png-chunk:IEND>))
         (image (make <png-image>
                  #:chunks    (list ihdr iend))))
    (png-image-height image)))


(define exit-status (test-runner-fail-count (test-runner-current)))

(test-end %test-name)

(exit exit-status)
