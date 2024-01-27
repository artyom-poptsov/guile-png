;;; blur.scm -- "Blur" (low-pass) filter implementation.

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

;; This module contains the implementation of "Blur" (low-pass) filter using
;; "Averaging" method.


;;; Code:

(define-module (png image-processing blur)
  #:use-module (oop goops)
  #:use-module (rnrs bytevectors)
  #:use-module (png image)
  #:use-module (png graphics pixel)
  #:use-module (png core error)
  #:use-module (png core chunk plte)
  #:export (png-image-blur))


;; 3x3 normalized box filter.

(define %blur-kernel
  #(#(0.11  0.11  0.11)
    #(0.11  0.11  0.11)
    #(0.11  0.11  0.11)))


(define-method (png-image-blur (image <png-image>))
  "Copy an @var{image} and apply 'blur' effect on the copy, using 'averaging'
method.

Return the new image.

The filtering is done using a simple low-pass filter.

The filter will not be applied to an @var{image} if it has 'indexed' color
type; an error will be issued instead."
  (define (kernel-ref y x)
    (vector-ref (vector-ref %blur-kernel y) x))

  (define (convolute dst-image bpp y x)
    (let k-loop ((k      -1)
                 (result (make-bytevector bpp)))
      (if (> k 1)
          result
          (begin
            (let j-loop ((j -1))
              (unless (> j 1)
                (let ((pixel (png-image-pixel-ref image
                                                  (- x k)
                                                  (- y j))))
                  (let loop ((idx 0))
                    (unless (= idx bpp)
                      (let ((value (+ (bytevector-u8-ref result idx)
                                      (* (kernel-ref (+ j 1) (+ k 1))
                                         (bytevector-u8-ref pixel idx)))))
                        (bytevector-u8-set! result
                                            idx
                                            (inexact->exact (floor value))))
                      (loop (+ idx 1)))))
                (j-loop (+ j 1))))
            (k-loop (+ k 1) result)))))

  (when (equal? (png-image-color-type/symbol image) 'indexed)
    (png-error "Unsupported 'indexed' color type for 'blur' operation"
               image))

  (let* ((image-clone (png-image-clone image))
         (width       (png-image-width image-clone))
         (height      (png-image-height image-clone))
         (bpp         (png-image-pixel-size image-clone)))
    (let y-loop ((y 1))
      (if (= y (- height 1))
          image-clone
          (begin
            (let x-loop ((x 1))
              (unless (= x (- width 1))
                (let ((result-pixel (convolute image-clone bpp y x)))
                  (png-image-pixel-set! image-clone x y result-pixel)
                  (x-loop (+ x 1)))))
            (y-loop (+ y 1)))))))

;;; blur.scm ends here.
