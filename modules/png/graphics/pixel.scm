;;; pixel.scm -- Low-level pixel manipulation.

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

;; This module contains low-level procedures for working PNG image pixels.


;;; Code:

(define-module (png graphics pixel)
  #:use-module (oop goops)
  #:use-module (rnrs bytevectors)
  #:use-module (png image)
  #:export (png-image-pixel-ref
            png-image-pixel-set!))



(define-method (png-image-pixel-ref (image <png-image>)
                                    (x     <number>))
  "Get a pixel of an IMAGE by its X position in the bytevector of the IMAGE
pixels."
  (let* ((data        (png-image-data image))
         (pixel-count (* (png-image-width image) (png-image-height image)))
         (bpp         (png-image-pixel-size image))
         (offset      (* x bpp))
         (result      (make-bytevector bpp 0)))
    (when (>= x pixel-count)
      (error "Pixel reference is outside the data" x pixel-count))
    (let loop ((index 0))
      (if (= index bpp)
          result
          (begin
            (bytevector-u8-set! result
                                index
                                (bytevector-u8-ref data (+ offset index)))
            (loop (+ index 1)))))))

(define-method (png-image-pixel-ref (image <png-image>)
                                    (x     <number>)
                                    (y     <number>))
  "Get a pixel of an IMAGE by its X and Y position on the IMAGE."
  (png-image-pixel-ref image
                       (+ (* y (png-image-width image))
                          x)))

(define-method (png-image-pixel-set! (image <png-image>)
                                     (x     <number>)
                                     (pixel <bytevector>))
  "Set a pixel of an IMAGE, referenced by the X position in the IMAGE bytevector
of pixels."
  (let* ((data        (png-image-data image))
         (pixel-count (* (png-image-width image) (png-image-height image)))
         (bpp         (png-image-pixel-size image))
         (offset      (* x bpp)))
    (when (>= x pixel-count)
      (error "Pixel reference is outside the data" x pixel-count))
    (let loop ((index 0))
      (unless (= index bpp)
        (bytevector-u8-set! data
                            (+ offset index)
                            (bytevector-u8-ref pixel index))
        (loop (+ index 1))))))

(define-method (png-image-pixel-set! (image <png-image>)
                                     (x     <number>)
                                     (y     <number>)
                                     (pixel <bytevector>))
  "Set a pixel of an IMAGE, referenced by the X and Y position in the IMAGE."
  (png-image-pixel-set! image
                        (+ (* y (png-image-width image))
                           x)
                        pixel))

;;; pixel.scm ends here.
