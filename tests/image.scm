(use-modules (srfi srfi-64)
             (srfi srfi-26)
             (oop goops)
             (png image)
             (png core chunk)
             (png core chunk-ihdr)
             (png core chunk-iend))


(define %test-name "image")


(test-begin %test-name)

(test-assert "png-image-chunks-query: IHDR"
  (let* ((ihdr  (make <png-chunk:IHDR>
                  #:type      (chunk-type->vector 'IHDR)))
         (iend  (make <png-chunk:IEND>))
         (image (make <png-image>
                  #:chunks    (list ihdr iend)))
         (chunks (png-image-chunks-query image 'IHDR)))
    (and (= (length chunks) 1)
         (car chunks))))


(define exit-status (test-runner-fail-count (test-runner-current)))

(test-end %test-name)

(exit exit-status)
