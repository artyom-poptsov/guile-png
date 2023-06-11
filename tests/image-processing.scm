(use-modules (srfi srfi-64)
             (srfi srfi-26)
             (rnrs bytevectors)
             (oop goops)
             (png)
             (png fsm context)
             (png image)
             (png image-processing))

(define %topdir (getenv "abs_top_srcdir"))
(define %meditate (format #f "~a/tests/meditate.png" %topdir))

(define %test-name "image-processing")



(define-method (configure-test-logging! (test-suite-name <string>))
  (smc-log-init! "file" `((file . ,(string-append test-suite-name "-smc.log")))))


(configure-test-logging! %test-name)
(test-begin %test-name)

(test-assert "grayscale"
  (let ((image (png->scm (open-input-file %meditate))))
    (png-image-grayscale image)))

(test-assert "invert-colors"
  (let ((image (png->scm (open-input-file %meditate))))
    (png-image-invert-colors image)))

(test-assert "solarize"
  (let ((image (png->scm (open-input-file %meditate))))
    (png-image-solarize image 5)))


(define exit-status (test-runner-fail-count (test-runner-current)))

(test-end %test-name)

(exit exit-status)
