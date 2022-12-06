(use-modules (srfi srfi-64)
             (srfi srfi-26)
             (oop goops)
             (png image)
             (png core chunk)
             (png core chunk IHDR)
             (png core chunk IEND))


(define %test-name "image")


(test-begin %test-name)

(test-assert "png-compressed-image?"
  (let* ((ihdr  (make <png-chunk:IHDR>
                  #:type      'IHDR))
         (iend  (make <png-chunk:IEND>))
         (image (make <png-compressed-image>
                  #:chunks    (list ihdr iend))))
    (png-compressed-image? image)))

(test-assert "png-compressed-image-chunks-query: IHDR"
  (let* ((ihdr  (make <png-chunk:IHDR>
                  #:type      'IHDR))
         (iend  (make <png-chunk:IEND>))
         (image (make <png-compressed-image>
                  #:chunks    (list ihdr iend)))
         (chunks (png-image-chunks-query image 'IHDR)))
    (and (= (length chunks) 1)
         (car chunks))))

(test-equal "png-image-width"
  200
  (let* ((ihdr  (make <png-chunk:IHDR>
                  #:width     200
                  #:height    100
                  #:type      'IHDR))
         (iend  (make <png-chunk:IEND>))
         (image (make <png-compressed-image>
                  #:chunks    (list ihdr iend))))
    (png-image-width image)))

(test-equal "png-image-height"
  100
  (let* ((ihdr  (make <png-chunk:IHDR>
                  #:width     200
                  #:height    100
                  #:type      'IHDR))
         (iend  (make <png-chunk:IEND>))
         (image (make <png-compressed-image>
                  #:chunks    (list ihdr iend))))
    (png-image-height image)))

(test-equal "png-image-bit-depth"
  8
  (let* ((ihdr  (make <png-chunk:IHDR>
                  #:width     200
                  #:height    100
                  #:bit-depth 8
                  #:type      'IHDR))
         (iend  (make <png-chunk:IEND>))
         (image (make <png-compressed-image>
                  #:chunks    (list ihdr iend))))
    (png-image-bit-depth image)))

(test-equal "png-image-color-type"
  0
  (let* ((ihdr  (make <png-chunk:IHDR>
                  #:width      200
                  #:height     100
                  #:bit-depth  8
                  #:color-type 0
                  #:type      'IHDR))
         (iend  (make <png-chunk:IEND>))
         (image (make <png-compressed-image>
                  #:chunks    (list ihdr iend))))
    (png-image-color-type image)))


(define exit-status (test-runner-fail-count (test-runner-current)))

(test-end %test-name)

(exit exit-status)
