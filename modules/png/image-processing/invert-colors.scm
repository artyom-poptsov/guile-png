;;; invert-colors.scm -- "Color inversion" filter implementation.

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


;;; Commentary:

;; This module contains the implementation of "Color inversion" filter.


;;; Code:

(define-module (png image-processing invert-colors)
  #:use-module (oop goops)
  #:use-module (rnrs bytevectors)
  #:use-module (png image)
  #:use-module (png graphics pixel)
  #:use-module (png core chunk plte)
  #:export (png-image-invert-colors))



(define-method (png-image-invert-colors (image <png-image>))
  "Copy an IMAGE and invert its colors.  Return the new image."
  (if (= (png-image-color-type image) 3)
      (let* ((image-clone   (png-image-clone image))
             (palette       (png-image-palette image-clone))
             (palette-count (vector-length palette)))
        (let loop ((index 0))
          (if (= index palette-count)
              image-clone
              (begin
                (let* ((color (vector-ref palette index))
                       (red   (bytevector-u8-ref color 0))
                       (green (bytevector-u8-ref color 1))
                       (blue  (bytevector-u8-ref color 2)))
                  (bytevector-u8-set! color 0 (- 255 red))
                  (bytevector-u8-set! color 1 (- 255 green))
                  (bytevector-u8-set! color 2 (- 255 blue))
                  (loop (+ index 1)))))))
      (png-image-pixel-map image
                           (lambda (index pixel)
                             (let ((red   (bytevector-u8-ref pixel 0))
                                   (green (bytevector-u8-ref pixel 1))
                                   (blue  (bytevector-u8-ref pixel 2)))
                               (bytevector-u8-set! pixel 0 (- 255 red))
                               (bytevector-u8-set! pixel 1 (- 255 green))
                               (bytevector-u8-set! pixel 2 (- 255 blue))
                               pixel)))))

;;; invert-colors.scm ends here.
