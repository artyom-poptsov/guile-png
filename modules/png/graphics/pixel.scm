;;; pixel.scm -- Low-level pixel manipulation.

;; Copyright (C) 2022-2024 Artyom V. Poptsov <poptsov.artyom@gmail.com>
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
            png-image-pixel-set!
            png-image-pixel-for-each
            png-image-pixel-map
            png-image-pixel-fold))



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
    (when (> bpp (bytevector-length pixel))
      (error "Bytes per pixel value mismatch" pixel bpp))
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


;;; High-level procedures.
(define-method (png-image-pixel-for-each (image <png-image>)
                                         (proc  <procedure>))
  "Apply @var{proc} to the each pixel of a @var{image} copy.  Return value is
undefined.

The @var{proc} is called like follows:
@example lisp
(proc pixel #:index index #:x x #:y y)
@end example
"
  (let ((pixel-count (png-image-pixels image))
        (width       (png-image-width image))
        (height      (png-image-height image)))
    (let loop ((index 0))
      (unless (= index pixel-count)
        (let ((pixel (png-image-pixel-ref image index))
              (x     (truncate-remainder index width))
              (y     (truncate-quotient index width)))
          (proc pixel #:index index #:x x #:y y)
          (loop (+ index 1)))))))

(define-method (png-image-pixel-map (image <png-image>)
                                    (proc  <procedure>))
  "Apply @var{proc} to the each pixel of a @var{image} copy.  Return the new
image.

The @var{proc} is called like follows:
@example lisp
(proc pixel #:index index #:x x #:y y)
@end example

The return value of the @var{proc} must be a new pixel.
"
  (let ((image-clone (png-image-clone image))
        (pixel-count (png-image-pixels image))
        (width       (png-image-width image))
        (height      (png-image-height image)))
    (let loop ((index 0))
      (if (= index pixel-count)
          image-clone
          (let ((pixel (png-image-pixel-ref image index))
                (x     (truncate-remainder index width))
                (y     (truncate-quotient index width)))
            (png-image-pixel-set! image-clone
                                  index
                                  (proc pixel #:index index #:x x #:y y))
            (loop (+ index 1)))))))

(define-method (png-image-pixel-fold (image <png-image>)
                                     (init  <top>)
                                     (proc  <procedure>))
  "Apply @var{proc} to the each pixel of an @var{image} to build a result,
and return that result.

Each @var{proc} call is
@example lisp
(proc pixel previous #:index index #:x x #:y y)
@end example

@code{previous} is the return from the previous @var{proc} call, or the given
@var{init} for the first call.
"
  (let ((pixel-count (png-image-pixels image))
        (width       (png-image-width image))
        (height      (png-image-height image)))
    (let loop ((index 0)
               (result init))
      (if (= index pixel-count)
          result
          (let ((pixel (png-image-pixel-ref image index))
                (x     (truncate-remainder index width))
                (y     (truncate-quotient index width)))
            (loop (+ index 1)
                  (proc pixel result #:index index #:x x #:y y)))))))

;;; pixel.scm ends here.
