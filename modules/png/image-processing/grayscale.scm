;;; grayscale.scm -- "Grayscale" filter implementation.

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

;; This module contains the implementation of "Grayscale" filter.


;;; Code:

(define-module (png image-processing grayscale)
  #:use-module (oop goops)
  #:use-module (rnrs bytevectors)
  #:use-module (png image)
  #:use-module (png graphics pixel)
  #:use-module (png core chunk plte)
  #:export (png-image-grayscale))



(define (%make-pixel-converter image method)
  (case method
    ((weighted)
     (lambda (red green blue)
       (inexact->exact
        (round
         (+ (* red 0.299)
            (* green 0.587)
            (* blue 0.114))))))
    ((average)
     (lambda (red green blue)
       (inexact->exact
        (round
         (+ (/ red 3) (/ green 3) (/ blue 3))))))
    (else
     (error "Unknown grayscale conversion method"
            image
            method))))



(define* (png-image-grayscale image
                              #:key
                              (method 'weighted))
  "Copy an IMAGE and apply 'grayscale' effect on the copy.  Return the new
image.

A METHOD is a symbol that is expected to be either 'weighted' (default) or
'average'."
  (let ((image-clone (png-image-clone image))
        (pixel-count (png-image-pixels image))
        (pixel-converter (%make-pixel-converter image method)))
    (if (= (png-image-color-type image) 3)
        (let* ((palette       (png-image-palette image-clone))
               (palette-count (vector-length palette)))
          (let loop ((index 0))
            (if (= index palette-count)
                image-clone
                (let* ((color (vector-ref palette index))
                       (red   (bytevector-u8-ref color 0))
                       (green (bytevector-u8-ref color 1))
                       (blue  (bytevector-u8-ref color 2))
                       (grayscale-value (pixel-converter red green blue)))
                  (bytevector-u8-set! color 0 grayscale-value)
                  (bytevector-u8-set! color 1 grayscale-value)
                  (bytevector-u8-set! color 2 grayscale-value)
                  (loop (+ index 1))))))
        (begin
          (slot-set! image-clone
                     'data
                     (make-bytevector pixel-count 0))
          (let loop ((index 0))
            (if (= index pixel-count)
                image-clone
                (let* ((pixel (png-image-pixel-ref image index))
                       (red   (bytevector-u8-ref pixel 0))
                       (green (bytevector-u8-ref pixel 1))
                       (blue  (bytevector-u8-ref pixel 2))
                       (grayscale-value (pixel-converter red green blue))
                       (new-pixel (make-bytevector 1 grayscale-value)))
                  (png-image-color-type-set! image-clone 0)
                  (png-image-pixel-set! image-clone index new-pixel)
                  (loop (+ index 1)))))))))

;;; grayscale.scm ends here.
