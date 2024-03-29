;;; image-processing.scm -- Image processing tests.

;; Copyright (C) 2023-2024 Artyom V. Poptsov <poptsov.artyom@gmail.com>
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; The program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with the program.  If not, see <http://www.gnu.org/licenses/>.


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
(define %example-ellipse (format #f "~a/tests/example-ellipse.png" %topdir))
(define %example-rainbow
  (format #f "~a/tests/example-rainbow.png" %topdir))

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

(test-assert "blur"
  (let ((image (png->scm (open-input-file %example-ellipse))))
    (png-image-blur image)))

(test-error "blur: 'indexed' color type is unsupported yet"
  (png-image-blur (make <png-image> #:color-type 'indexed)))



(test-assert "rotate: 90 degrees clockwise"
  (let ((image (png->scm (open-input-file %example-rainbow))))
    (png-image-rotate-90/cw image)))

(test-assert "rotate: 90 degrees counter-clockwise"
  (let ((image (png->scm (open-input-file %example-rainbow))))
    (png-image-rotate-90/ccw image)))

(test-assert "rotate: 180"
  (let ((image (png->scm (open-input-file %example-rainbow))))
    (png-image-rotate-180 image)))


(define exit-status (test-runner-fail-count (test-runner-current)))

(test-end %test-name)

(exit exit-status)
