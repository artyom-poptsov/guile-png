;;; image-processing.scm -- Image processing procedures.

;; Copyright (C) 2022 Artyom V. Poptsov <poptsov.artyom@gmail.com>
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

;; This module contains image processing procedures.


;;; Code:

(define-module (png image-processing)
  #:use-module (oop goops)
  #:use-module (rnrs bytevectors)
  #:use-module (png image)
  #:use-module (png pixel)
  #:export (png-image-filter-invert-colors
            png-image-filter-solarize))


(define-method (png-image-filter-invert-colors (image <png-image>))
  "Copy IMAGE and its colors.  Return new image."
  (let* ((image-clone (png-image-clone image))
         (pixel-count (png-image-pixels image)))
    (let loop ((index 0))
      (if (= index pixel-count)
          image-clone
          (begin
            (let* ((pixel (png-image-pixel-ref image index))
                   (red   (bytevector-u8-ref pixel 0))
                   (green (bytevector-u8-ref pixel 1))
                   (blue  (bytevector-u8-ref pixel 2)))
              (bytevector-u8-set! pixel 0 (- 255 red))
              (bytevector-u8-set! pixel 1 (- 255 green))
              (bytevector-u8-set! pixel 2 (- 255 blue))
              (png-image-pixel-set! image-clone index pixel)
              (loop (+ index 1))))))))

(define-method (png-image-filter-solarize (image     <png-image>)
                                          (threshold <number>))
  (let ((image-clone (png-image-clone image))
        (pixel-count (png-image-pixels image)))
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
            (loop (+ index 1)))))))

;;; image-processing.scm ends here.
