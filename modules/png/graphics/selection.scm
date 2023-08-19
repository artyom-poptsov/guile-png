;;; selection.scm -- Selected part of an image.

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

;; This module contains definition of the <selection> class that represents a
;; selected part of an image.


;;; Code:

(define-module (png graphics selection)
  #:use-module (oop goops)
  #:use-module (png core common)
  #:use-module (png graphics point)
  #:use-module (png graphics dimension)
  #:use-module (png image)
  #:export (<selection>
            selection?
            selection-image
            selection-position
            selection-dimension

            png-image-select))



(define-class <selection> ()
  ;; <png-image>
  (image
   #:init-value   #f
   #:init-keyword #:image
   #:getter       selection-image)

  ;; <point>
  (position
   #:init-thunk   (lambda () (make <point>))
   #:init-keyword #:position
   #:getter       selection-position)

  ;; <dimension>
  (dimension
   #:init-thunk   (lambda () (make <dimension>))
   #:init-keyword #:dimension
   #:getter       selection-dimension))



(define (selection? x)
  "Check if X is a <selection> instance."
  (is-a? x <selection>))


(define-method (%display (selection <selection>) (port <port>))
  (format port "#<selection image: ~a position: ~a dimension: ~a ~a>"
          (selection-image selection)
          (selection-position selection)
          (selection-dimension selection)
          (object-address/hex-string selection)))

(define-method (display (selection <selection>) (port <port>))
  (%display selection port))

(define-method (write (selection <selection>) (port <port>))
  (%display selection port))



(define-method (png-image-select (image <png-image>)
                                 (position <point>)
                                 (dimension <dimension>))
  "Select a part of an IMAGE with the specified POSITION and DIMENSION.  Return
a new selection object.  Throw an error when the selected area is outside an
IMAGE."
  (let ((img-width  (png-image-width image))
        (img-height (png-image-height image)))

    (when (or (< (point-x position) 0)
              (>= (point-x position) img-width))
      (error "Selection X position is outside an image"
             image
             position))

    (when (or (< (point-y position) 0)
              (>= (point-y position) img-height))
      (error "Selection Y position is outside an image"
             image
             position))

    (when (>= (+ (point-x position) (dimension-width dimension))
              img-width)
      (error "Selection width is outside the image dimensions"
             image
             position
             dimension))

    (when (>= (+ (point-y position) (dimension-height dimension))
              img-height)
      (error "Selection height is outside the image dimensions"
             image
             position
             dimension))

    (make <selection>
      #:image     image
      #:position  position
      #:dimension dimension)))

;;; selection.scm ends here.
