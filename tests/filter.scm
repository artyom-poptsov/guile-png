(use-modules (srfi srfi-64)
             (srfi srfi-26)
             (rnrs bytevectors)
             (oop goops)
             (png core filter))


(define %test-name "filter")

(test-begin %test-name)



(test-equal "png-filter-algorithm-type->name"
  'NONE
  (png-filter-algorithm-type->name 0))

(test-equal "png-filter-algorithm-name->type"
  0
  (png-filter-algorithm-name->type 'NONE))


(define %test-image-data
  #vu8(255 255 255 255
       0   0   0   0
       255 255 255 255
       0   0   0   0))

(test-equal "png-filter-apply!: <png-filter:none>"
  #vu8(0 255 255 255 255
       0 0   0   0   0
       0 255 255 255 255
       0 0   0   0   0)
  (let ((filter (make <png-filter:none>
                  #:scanline-length 4
                  #:bytes-per-pixel 1))
        (result (make-bytevector (+ (bytevector-length %test-image-data) 4))))
    (let loop ((index 0))
      (if (= index 4)
          result
          (begin
            (png-filter-apply! filter %test-image-data result index)
            (loop (+ index 1)))))))

(test-equal "png-filter-apply!: <png-filter:sub>"
  #vu8(1 255 0 0 0
       1 0   0 0 0
       1 255 0 0 0
       1 0   0 0 0)
  (let ((filter (make <png-filter:sub>
                  #:scanline-length 4
                  #:bytes-per-pixel 1))
        (result (make-bytevector (+ (bytevector-length %test-image-data) 4))))
    (let loop ((index 0))
      (if (= index 4)
          result
          (begin
            (png-filter-apply! filter %test-image-data result index)
            (loop (+ index 1)))))))

(test-equal "png-filter-apply!: <png-filter:up>"
  #vu8(2 255 255 255 255
       2 1   1   1   1
       2 255 255 255 255
       2 1   1   1   1)
  (let ((filter (make <png-filter:up>
                   #:scanline-length 4
                   #:bytes-per-pixel 1))
        (result (make-bytevector (+ (bytevector-length %test-image-data) 4))))
    (let loop ((index 0))
      (if (= index 4)
          result
          (begin
            (png-filter-apply! filter %test-image-data result index)
            (loop (+ index 1)))))))


(define exit-status (test-runner-fail-count (test-runner-current)))

(test-end %test-name)

(exit exit-status)
