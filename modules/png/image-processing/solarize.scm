;;; solarize.scm -- "Solarize" filter implementation.

;; Copyright (C) 2023 Artyom V. Poptsov <poptsov.artyom@gmail.com>
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

;; This module contains the implementation of "Solarize" filter.


;;; Code:

(define-module (png image-processing solarize)
  #:use-module (oop goops)
  #:use-module (rnrs bytevectors)
  #:use-module (png image)
  #:use-module (png graphics pixel)
  #:use-module (png core chunk plte)
  #:use-module (png image-processing grayscale)
  #:export (png-image-solarize))



(define-method (png-image-solarize (image     <png-image>)
                                   (threshold <number>))
  "Copy an IMAGE and apply 'solarize' effect on the copy.  Return the new
image."
  (let ((image-clone (png-image-clone image))
        (pixel-count (png-image-pixels image)))
    (if (= (png-image-color-type image) 3)
        (let* ((palette       (png-image-palette image-clone))
               (palette-count (vector-length palette)))
          (let loop ((index 0))
            (if (= index palette-count)
                image-clone
                (let* ((color (vector-ref palette index))
                       (red   (bytevector-u8-ref color 0))
                       (green (bytevector-u8-ref color 1))
                       (blue  (bytevector-u8-ref color 2)))
                  (when (< red threshold)
                    (bytevector-u8-set! color 0 (- 255 red)))
                  (when (< green threshold)
                    (bytevector-u8-set! color 1 (- 255 green)))
                  (when (< blue threshold)
                    (bytevector-u8-set! color 2 (- 255 blue)))
                  (loop (+ index 1))))))
        (let loop ((index 0))
          (if (= index pixel-count)
              image-clone
              (let* ((pixel (png-image-pixel-ref image index))
                     (red   (bytevector-u8-ref pixel 0))
                     (green (bytevector-u8-ref pixel 1))
                     (blue  (bytevector-u8-ref pixel 2)))
                (when (< red threshold)
                  (bytevector-u8-set! pixel 0 (- 255 red)))
                (when (< green threshold)
                  (bytevector-u8-set! pixel 1 (- 255 green)))
                (when (< blue threshold)
                  (bytevector-u8-set! pixel 2 (- 255 blue)))
                (png-image-pixel-set! image-clone index pixel)
                (loop (+ index 1))))))))

;;; solarize.scm ends here.
