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
  #:use-module (rnrs bytevectors)
  #:use-module (oop goops)
  #:use-module (png core common)
  #:use-module (png graphics point)
  #:use-module (png graphics dimension)
  #:use-module (png image)
  #:export (<selection>
            selection?
            selection-image
            selection-position
            selection-size

            selection-crop

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
  (size
   #:init-thunk   (lambda () (make <dimension>))
   #:init-keyword #:size
   #:getter       selection-size))



(define (selection? x)
  "Check if X is a <selection> instance."
  (is-a? x <selection>))


(define-method (%display (selection <selection>) (port <port>))
  (format port "#<selection image: ~a position: ~a size: ~a ~a>"
          (selection-image selection)
          (selection-position selection)
          (selection-size selection)
          (object-address/hex-string selection)))

(define-method (display (selection <selection>) (port <port>))
  (%display selection port))

(define-method (write (selection <selection>) (port <port>))
  (%display selection port))



(define-method (png-image-select (image <png-image>)
                                 (position <point>)
                                 (size <dimension>))
  "Select a part of an IMAGE with the specified POSITION and SIZE.  Return
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

    (when (> (+ (point-x position) (dimension-width size))
              img-width)
      (error "Selection width is outside the image dimensions"
             image
             position
             size))

    (when (> (+ (point-y position) (dimension-height size))
             img-height)
      (error "Selection height is outside the image dimensions"
             image
             position
             size))

    (make <selection>
      #:image    image
      #:position position
      #:size     size)))



(define-method (selection-crop (selection <selection>))
  "Crop a part of an image to the SELECTION part.  Return a new image."
  (let* ((img        (selection-image selection))
         (img-width  (png-image-width img))
         (img-height (png-image-height img))
         (pixel-size (png-image-pixel-size img))
         (pos        (selection-position selection))
         (size       (selection-size selection))
         (data       (png-image-data img))
         (result     (make-bytevector (* (* (dimension-width size)
                                            (dimension-height size))
                                         pixel-size)))
         (image-copy (png-image-clone img)))
    (png-image-data-set! image-copy result)
    (png-image-width-set! image-copy (dimension-width size))
    (png-image-height-set! image-copy (dimension-height size))
    (for-each (lambda (row-index)
                (let ((offset (* row-index
                                 (dimension-width size)
                                 pixel-size))
                      (source-offset (+ (* (+ (point-y pos)
                                              row-index)
                                           img-width
                                           pixel-size)
                                        (* (point-x pos)
                                           pixel-size)))
                      (source-length (* (dimension-width size)
                                        pixel-size)))
                  (bytevector-copy/part! data
                                         result
                                         #:source-offset source-offset
                                         #:source-length source-length
                                         #:target-offset offset)))
              (iota (dimension-height size)))
    image-copy))

;;; selection.scm ends here.
