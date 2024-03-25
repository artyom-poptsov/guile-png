;;; rotate.scm -- Image rotation methods.

;; Copyright (C) 2024 Artyom V. Poptsov <poptsov.artyom@gmail.com>
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


;;; Commentary:

;; This module contains the implementation of image rotation methods.


;;; Code:


(define-module (png image-processing rotate)
  #:use-module (oop goops)
  #:use-module (rnrs bytevectors)
  #:use-module (png image)
  #:use-module (png graphics pixel)
  #:use-module (png core error)
  #:export (png-image-rotate-90/ccw
            png-image-rotate-90/cw))



(define-method (png-image-rotate-90/ccw (image <png-image>))
  "Rotate an IMAGE counter-clockwise by the specified ANGLE.  Return the new
image."
  (let* ((image-clone (png-image-clone image))
         (height      (png-image-height image))
         (depth       (/ height 2)))
    (let layer-loop ((layer-index 0))
      (if (= layer-index depth)
          image-clone
          (let ((len           (- height (* 2 layer-index) 1))
                (opposite-side (- height 1 layer-index)))
            (let pixel-loop ((pixel-index 0))
              (when (< pixel-index len)
                (let ((pixel (png-image-pixel-ref image-clone
                                                  layer-index
                                                  (+ layer-index
                                                     pixel-index))))
                  (png-image-pixel-set! image-clone
                                        layer-index
                                        (+ layer-index
                                           pixel-index)
                                        (png-image-pixel-ref image-clone
                                                             (- opposite-side
                                                                pixel-index)
                                                             layer-index))
                  (png-image-pixel-set! image-clone
                                        (- opposite-side pixel-index)
                                        layer-index
                                        (png-image-pixel-ref image-clone
                                                             opposite-side
                                                             (- opposite-side
                                                                pixel-index)))
                  (png-image-pixel-set! image-clone
                                        opposite-side
                                        (- opposite-side pixel-index)
                                        (png-image-pixel-ref image-clone
                                                             (+ layer-index
                                                                pixel-index)
                                                             opposite-side))
                  (png-image-pixel-set! image-clone
                                        (+ layer-index pixel-index)
                                        opposite-side
                                        pixel))
                (pixel-loop (+ pixel-index 1))))
            (layer-loop (+ layer-index 1)))))))

(define-method (png-image-rotate-90/cw (image <png-image>))
  "Rotate an IMAGE clockwise by the specified ANGLE.  Return the new
image."
  (let* ((image-clone (png-image-clone image))
         (height      (png-image-height image))
         (width       (png-image-width image))
         (depth       (/ height 2)))
    (let layer-loop ((layer-index 0))
      (if (= layer-index depth)
          image-clone
          (let ((len           (- height (* 2 layer-index) 1))
                (opposite-side (- height 1 layer-index)))
            (let pixel-loop ((pixel-index 0))
              (when (< pixel-index len)
                (let ((pixel (png-image-pixel-ref image-clone
                                                  (+ layer-index
                                                     pixel-index)
                                                  layer-index)))
                  (png-image-pixel-set! image-clone
                                        (+ layer-index
                                           pixel-index)
                                        layer-index
                                        (png-image-pixel-ref image-clone
                                                             layer-index
                                                             (- opposite-side
                                                                pixel-index)))
                  (png-image-pixel-set! image-clone
                                        layer-index
                                        (- opposite-side pixel-index)
                                        (png-image-pixel-ref image-clone
                                                             (- opposite-side
                                                                pixel-index)
                                                             opposite-side))
                  (png-image-pixel-set! image-clone
                                        (- opposite-side pixel-index)
                                        opposite-side
                                        (png-image-pixel-ref image-clone
                                                             opposite-side
                                                             (+ layer-index
                                                                pixel-index)))
                  (png-image-pixel-set! image-clone
                                        opposite-side
                                        (+ layer-index pixel-index)
                                        pixel))
                (pixel-loop (+ pixel-index 1))))
            (layer-loop (+ layer-index 1)))))))

;;; rotate.scm ends here.
