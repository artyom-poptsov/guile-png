(use-modules (srfi srfi-64)
             (srfi srfi-26)
             (ice-9 iconv)
             (rnrs bytevectors)
             (scheme base)
             (oop goops)
             (png image)
             (png core chunk)
             (png core chunk ihdr)
             (png core chunk iend)
             (png core chunk text))


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

(test-assert "<png-image>: png-image-clone"
  (let* ((ihdr  (make <png-chunk:IHDR>
                  #:width     200
                  #:height    100
                  #:bit-depth 8
                  #:color-type 8
                  #:type      'IHDR))
         (iend  (make <png-chunk:IEND>))
         (image (make <png-image>
                  #:chunks    (list ihdr iend)
                  #:data      #vu8(255 255 255 255)))
         (image-clone (png-image-clone image)))
    (and (not (eq? (png-image-chunks image)
                   (png-image-chunks image-clone)))
         (not (eq? (png-image-data image)
                   (png-image-data image-clone))))))

(test-equal "png-image-data/apply-filter"
  (+ (* 100 100 3) 100) ; 100x100 size * 3b per pixel + 100b filtering info
  (let ((image (make <png-image>
                 #:color-type 2
                 #:bit-depth  8
                 #:width      100
                 #:height     100)))
    (bytevector-length (png-image-data/apply-filter image))))

(test-equal "png-image-chunks-insert!: after 0 (chunk list is empty)"
  #f
  (let ((image (make <png-image>
                 #:color-type 2
                 #:bit-depth  8
                 #:width      100
                 #:height     100)))
    (png-image-chunks-insert! image 'after 0
                              (make <png-chunk:tEXt>
                                #:keyword "Title"
                                #:text    "Test Image"))))

(test-equal "png-image-chunks-insert!: before 0"
  'tEXt
  (let ((image (make <png-image>
                 #:color-type 2
                 #:bit-depth  8
                 #:width      100
                 #:height     100)))
    (png-image-chunks-insert! image 'before 0
                              (make <png-chunk:tEXt>
                                #:keyword "Title"
                                #:text    "Test Image"))
    (png-chunk-type (car (png-image-chunks image)))))

(test-equal "png-image-chunks-insert!: after tEXt"
  "Eva Lu Ator"
  (let ((image (make <png-image>
                 #:color-type 2
                 #:bit-depth  8
                 #:width      100
                 #:height     100)))
    (png-image-chunks-insert! image 'before 0
                              (make <png-chunk:tEXt>
                                #:keyword "Title"
                                #:text    "Test Image"))
    (png-image-chunks-insert! image 'after 'tEXt
                              (make <png-chunk:tEXt>
                                #:keyword "Author"
                                #:text    "Eva Lu Ator"))
    (png-chunk:tEXT-text (cadr (png-image-chunks image)))))

(test-assert "png-image-chunks-insert!: unknown chunk"
  (let ((image (make <png-image>
                 #:color-type 2
                 #:bit-depth  8
                 #:width      100
                 #:height     100))
        (data (string->bytevector "test" "ASCII")))

    (png-image-chunks-insert! image 'before 0
                              (make <png-chunk>
                                #:length (bytevector-length data)
                                #:type   'tESt
                                #:data   data))

    (call-with-output-string
      (lambda (p)
        (png-image->png image p)))))



(test-assert "png-image->bytevector"
  (let ((image (make <png-image>
                 #:color-type 2
                 #:bit-depth  8
                 #:width      100
                 #:height     100)))
    (png-image->bytevector image)))


(define exit-status (test-runner-fail-count (test-runner-current)))

(test-end %test-name)

(exit exit-status)
