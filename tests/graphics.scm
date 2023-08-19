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

(test-assert "<selection>: image-select"
  (let ((image (make <png-image>
                 #:width  100
                 #:height 100
                 #:bit-depth 8
                 #:color-type 2)))
    (image-select image
                  (make <point>
                    #:x 10
                    #:y 20)
                  (make <dimension>
                    #:width  10
                    #:height 10))))

(test-error "<selection>: image-select: error: X outside image 1"
  #t
  (let ((image (make <png-image>
                 #:width  100
                 #:height 100
                 #:bit-depth 8
                 #:color-type 2)))
    (image-select image
                  (make <point>
                    #:x 101
                    #:y 20)
                  (make <dimension>
                    #:width  10
                    #:height 10))))

(test-error "<selection>: image-select: error: X outside image 2"
  #t
  (let ((image (make <png-image>
                 #:width  100
                 #:height 100
                 #:bit-depth 8
                 #:color-type 2)))
    (image-select image
                  (make <point>
                    #:x -1
                    #:y 20)
                  (make <dimension>
                    #:width  10
                    #:height 10))))

(test-error "<selection>: image-select: error: Y outside image 1"
  #t
  (let ((image (make <png-image>
                 #:width  100
                 #:height 100
                 #:bit-depth 8
                 #:color-type 2)))
    (image-select image
                  (make <point>
                    #:x 10
                    #:y 200)
                  (make <dimension>
                    #:width  10
                    #:height 10))))

(test-error "<selection>: image-select: error: Y outside image 2"
  #t
  (let ((image (make <png-image>
                 #:width  100
                 #:height 100
                 #:bit-depth 8
                 #:color-type 2)))
    (image-select image
                  (make <point>
                    #:x 10
                    #:y -200)
                  (make <dimension>
                    #:width  10
                    #:height 10))))

(test-error "<selection>: image-select: error: with outside image"
  #t
  (let ((image (make <png-image>
                 #:width  100
                 #:height 100
                 #:bit-depth 8
                 #:color-type 2)))
    (image-select image
                  (make <point>
                    #:x 10
                    #:y 20)
                  (make <dimension>
                    #:width  1000
                    #:height 10))))

(test-error "<selection>: image-select: error: height outside image"
  #t
  (let ((image (make <png-image>
                 #:width  100
                 #:height 100
                 #:bit-depth 8
                 #:color-type 2)))
    (image-select image
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


;; Rectangle.

(test-assert "<rectangle>: display"
  (with-output-to-string
    (lambda ()
      (display
       (make <rectangle>
         #:position (make <point> #:x 100 #:y 200)
         #:width    50
         #:height   100)))))


;; Rectangle.

(test-assert "<multiline>: display"
  (with-output-to-string
    (lambda ()
      (display
       (make <multiline>
         #:points (list (make <point> #:x 10 #:y 20)
                        (make <point> #:x 30 #:y 40)
                        (make <point> #:x 50 #:y 60)))))))


(define exit-status (test-runner-fail-count (test-runner-current)))

(test-end %test-name)

(exit exit-status)
