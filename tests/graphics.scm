(use-modules (srfi srfi-64)
             (srfi srfi-26)
             (rnrs bytevectors)
             (oop goops)
             (png)
             (png fsm context)
             (png image)
             (png graphics)
             (png fsm context))

(define %topdir (getenv "abs_top_srcdir"))
(define %example-rainbow (format #f "~a/tests/example-rainbow.png" %topdir))
(define %example-ellipse (format #f "~a/tests/example-ellipse.png" %topdir))
(define %example-rectangle (format #f "~a/tests/example-rectangle.png" %topdir))

(define %test-name "graphics")



(define-method (configure-test-logging! (test-suite-name <string>))
  (smc-log-init! "file" `((file . ,(string-append test-suite-name "-smc.log")))))


(configure-test-logging! %test-name)
(test-begin %test-name)


;; Dimension.

(test-assert "<dimension>: display"
  (with-output-to-string
    (lambda ()
      (display
       (make <dimension>
         #:width    50
         #:height   100)))))


;; Selection.

(test-assert "<selection>: display"
  (with-output-to-string
    (lambda ()
      (display
       (make <selection>
         #:image (make <png-image>
                   #:width  100
                   #:height 100
                   #:bit-depth 8
                   #:color-type 2)
         #:position (make <point>
                      #:x 10
                      #:y 20)
         #:dimension (make <dimension>
                       #:width  20
                       #:height 20))))))

(test-assert "<selection>: png-image-select"
  (let ((image (make <png-image>
                 #:width  100
                 #:height 100
                 #:bit-depth 8
                 #:color-type 2)))
    (png-image-select image
                      (make <point>
                        #:x 10
                        #:y 20)
                      (make <dimension>
                        #:width  10
                        #:height 10))))

(test-error "<selection>: png-image-select: error: X outside image 1"
  #t
  (let ((image (make <png-image>
                 #:width  100
                 #:height 100
                 #:bit-depth 8
                 #:color-type 2)))
    (png-image-select image
                      (make <point>
                        #:x 101
                        #:y 20)
                      (make <dimension>
                        #:width  10
                        #:height 10))))

(test-error "<selection>: png-image-select: error: X outside image 2"
  #t
  (let ((image (make <png-image>
                 #:width  100
                 #:height 100
                 #:bit-depth 8
                 #:color-type 2)))
    (png-image-select image
                      (make <point>
                        #:x -1
                        #:y 20)
                      (make <dimension>
                        #:width  10
                        #:height 10))))

(test-error "<selection>: png-image-select: error: Y outside image 1"
  #t
  (let ((image (make <png-image>
                 #:width  100
                 #:height 100
                 #:bit-depth 8
                 #:color-type 2)))
    (png-image-select image
                      (make <point>
                        #:x 10
                        #:y 200)
                      (make <dimension>
                        #:width  10
                        #:height 10))))

(test-error "<selection>: png-image-select: error: Y outside image 2"
  #t
  (let ((image (make <png-image>
                 #:width  100
                 #:height 100
                 #:bit-depth 8
                 #:color-type 2)))
    (png-image-select image
                      (make <point>
                        #:x 10
                        #:y -200)
                      (make <dimension>
                        #:width  10
                        #:height 10))))

(test-error "<selection>: png-image-select: error: with outside image"
  #t
  (let ((image (make <png-image>
                 #:width  100
                 #:height 100
                 #:bit-depth 8
                 #:color-type 2)))
    (png-image-select image
                      (make <point>
                        #:x 10
                        #:y 20)
                      (make <dimension>
                        #:width  1000
                        #:height 10))))

(test-error "<selection>: png-image-select: error: height outside image"
  #t
  (let ((image (make <png-image>
                 #:width  100
                 #:height 100
                 #:bit-depth 8
                 #:color-type 2)))
    (png-image-select image
                      (make <point>
                        #:x 10
                        #:y 20)
                      (make <dimension>
                        #:width  10
                        #:height 1000))))



(test-assert "rainbow 10x7"
  (let ((image  (make <png-image>
                   #:width 10
                   #:height 7
                   #:bit-depth 8
                   #:color-type 2))
        (test-image (png->scm (open-input-file %example-rainbow))))
    (draw! image
           (make <line>
             #:color #vu8(255 0 0 0)
             #:p1 (make <point> #:x 0 #:y 0)
             #:p2 (make <point> #:x 9 #:y 0)))
    (draw! image
           (make <line>
             #:color #vu8(255 165 0 0)
             #:p1 (make <point> #:x 0 #:y 1)
             #:p2 (make <point> #:x 9 #:y 1)))
    (draw! image
           (make <line>
             #:color #vu8(255 255 0 0)
             #:p1 (make <point> #:x 0 #:y 2)
             #:p2 (make <point> #:x 9 #:y 2)))
    (draw! image
           (make <line>
             #:color #vu8(0 255 0 0)
             #:p1 (make <point> #:x 0 #:y 3)
             #:p2 (make <point> #:x 9 #:y 3)))
    (draw! image
           (make <line>
             #:color #vu8(0 191 255 0)
             #:p1 (make <point> #:x 0 #:y 4)
             #:p2 (make <point> #:x 9 #:y 4)))
    (draw! image
           (make <line>
             #:color #vu8(0 0 255 0)
             #:p1 (make <point> #:x 0 #:y 5)
             #:p2 (make <point> #:x 9 #:y 5)))
    (draw! image
           (make <line>
             #:color #vu8(148 0 211 0)
             #:p1 (make <point> #:x 0 #:y 6)
             #:p2 (make <point> #:x 9 #:y 6)))
    (let loop ((bv1 (png-image-data image))
               (bv2 (png-image-data test-image))
               (index 0))
      (when (< index (bytevector-length bv1))
        (unless (equal? (bytevector-u8-ref bv1 index)
                        (bytevector-u8-ref bv2 index))
          (let ((p (open-output-file (format #f
                                             "~a/tests/graphics-errors.log"
                                             %topdir))))
            (display "generated image:\n" p)
            (png-image-pretty-print-data image p)
            (display "test image:\n" p)
            (png-image-pretty-print-data test-image p)
            (close p))
          (let ((p (open-output-file (format #f
                                             "~a/tests/graphics-rainbow-test-10x7.png"))))
            (scm->png image p)
            (close p))
          (error "Bytevectors are not equal" bv1 bv2 index))
        (loop bv1 bv2 (+ index 1))))))


;; Graphic.

(test-assert "<graphic>: display"
  (with-output-to-string
    (lambda ()
      (display
       (make <graphic>
         #:color #vu8(255 255 255))))))


;; Rectangle.

(test-assert "<rectangle>: display"
  (with-output-to-string
    (lambda ()
      (display
       (make <rectangle>
         #:position (make <point> #:x 100 #:y 200)
         #:width    50
         #:height   100)))))

(test-assert "<square>: display"
  (with-output-to-string
    (lambda ()
      (display
       (make <square>
         #:position (make <point> #:x 100 #:y 200)
         #:size    50)))))


;; Filled rectangle.

(test-assert "<filled-rectangle>: display"
  (with-output-to-string
    (lambda ()
      (display
       (make <filled-rectangle>
         #:position (make <point> #:x 100 #:y 200)
         #:width    50
         #:height   100)))))


;; Multiline.

(test-assert "<multiline>: display"
  (with-output-to-string
    (lambda ()
      (display
       (make <multiline>
         #:points (list (make <point> #:x 10 #:y 20)
                        (make <point> #:x 30 #:y 40)
                        (make <point> #:x 50 #:y 60)))))))


;; Ellipse.

(test-assert "<ellipse>: display"
  (with-output-to-string
    (lambda ()
      (display
       (make <ellipse>
         #:center (make <point> #:x 10 #:y 20)
         #:width  10
         #:height 20)))))

(define (draw-axis! image)
  (draw! image
         (make <line>
           #:color #vu8(255 0 0)
           #:p1 (make <point>
                  #:x 0
                  #:y (/ (png-image-height image) 2))
           #:p2 (make <point>
                  #:x (- (png-image-width image) 1)
                  #:y (/ (png-image-height image) 2))))
  (draw! image
         (make <line>
           #:color #vu8(255 0 0)
           #:p1 (make <point>
                  #:x (/ (png-image-width image) 2)
                  #:y 0)
           #:p2 (make <point>
                  #:x (/ (png-image-width image) 2)
                  #:y (- (png-image-height image) 1)))))

(test-assert "ellipse center: 50,50, size: 50x25"
  (let ((image  (make <png-image>
                   #:width      100
                   #:height     100
                   #:bit-depth  8
                   #:color-type 2))
        (test-image (png->scm (open-input-file %example-ellipse))))
    (draw-axis! image)
    (draw! image
           (make <ellipse>
             #:center (make <point> #:x 50 #:y 50)
             #:width  50
             #:height 25
             #:color  #vu8(255 255 255)))

    (let loop ((bv1 (png-image-data image))
               (bv2 (png-image-data test-image))
               (index 0))
      (when (< index (bytevector-length bv1))
        (unless (equal? (bytevector-u8-ref bv1 index)
                        (bytevector-u8-ref bv2 index))
          (let ((p (open-output-file (format #f
                                             "~a/tests/graphics-ellipse-errors.log"
                                             %topdir))))
            (display "generated image:\n" p)
            (png-image-pretty-print-data image p)
            (display "test image:\n" p)
            (png-image-pretty-print-data test-image p)
            (close p))
          (let ((p (open-output-file (format #f
                                             "~a/tests/graphics-ellipse-test-10x7.png"
                                             %topdir))))
            (scm->png image p)
            (close p))
          (error "Bytevectors are not equal" bv1 bv2 index))
        (loop bv1 bv2 (+ index 1))))))

(test-assert "rectangle position: 25,25, size: 50x50"
  (let ((image  (make <png-image>
                   #:width      100
                   #:height     100
                   #:bit-depth  8
                   #:color-type 2))
        (test-image (png->scm (open-input-file %example-rectangle))))
    (draw-axis! image)
    (draw! image
           (make <rectangle>
             #:position (make <point> #:x 25 #:y 25)
             #:width  50
             #:height 50
             #:color  #vu8(255 255 255)))

    (let loop ((bv1   (png-image-data image))
               (bv2   (png-image-data test-image))
               (index 0))
      (when (< index (bytevector-length bv1))
        (unless (equal? (bytevector-u8-ref bv1 index)
                        (bytevector-u8-ref bv2 index))
          (let ((p (open-output-file (format #f
                                             "~a/tests/graphics-rectangle-errors.log"
                                             %topdir))))
            (display "generated image:\n" p)
            (png-image-pretty-print-data image p)
            (display "test image:\n" p)
            (png-image-pretty-print-data test-image p)
            (close p))
          (let ((p (open-output-file (format #f
                                             "~a/tests/graphics-rectangle-test.png"
                                             %topdir))))
            (scm->png image p)
            (close p))
          (error "Bytevectors are not equal" bv1 bv2 index))
        (loop bv1 bv2 (+ index 1))))))


;; Circle.

(test-assert "<circle>: display"
  (with-output-to-string
    (lambda ()
      (display
       (make <circle>
         #:center (make <point> #:x 10 #:y 20)
         #:radius 10)))))


;; Regular polygons.

(test-assert "<regular-polygon>: display"
  (with-output-to-string
    (lambda ()
      (display
       (make <regular-polygon>
         #:center   (make <point> #:x 10 #:y 20)
         #:sides    6
         #:diameter 10)))))

(test-equal "<regular-polygon>: points"
  6
  (let ((p (make <regular-polygon>
             #:center   (make <point> #:x 10 #:y 20)
             #:sides    6
             #:diameter 10)))
    (length (regular-polygon-points p))))


(define exit-status (test-runner-fail-count (test-runner-current)))

(test-end %test-name)

(exit exit-status)
