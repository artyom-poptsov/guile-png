;;; filled-rectangle.scm -- Filled rectangle implementation.

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

;; This module contains the implementation of filled rectangle.


;;; Code:

(define-module (png graphics filled-rectangle)
  #:use-module (oop goops)
  #:use-module (png image)
  #:use-module (png graphics graphic)
  #:use-module (png graphics pixel)
  #:use-module (png graphics point)
  #:use-module (png graphics rectangle)
  #:export (<filled-rectangle>
            filled-rectangle-position
            filled-rectangle-width
            filled-rectangle-height))


(define-class <filled-rectangle> (<rectangle>))

(define filled-rectangle-position rectangle-position)
(define filled-rectangle-height   rectangle-height)
(define filled-rectangle-width    rectangle-width)


(define-method (draw! (image <png-image>) (rectangle <filled-rectangle>))
  (let* ((position   (rectangle-position rectangle))
         (position-x (point-x position))
         (position-y (point-y position))
         (width      (rectangle-width rectangle))
         (height     (rectangle-height rectangle))
         (color      (graphic-color rectangle)))
    (for-each (lambda (y)
                (for-each (lambda (x)
                            (png-image-pixel-set! image
                                                  (+ position-x x)
                                                  (+ position-y y)
                                                  color))
                          (iota width)))
              (iota height))))

;;; filled-rectangle.scm ends here.
