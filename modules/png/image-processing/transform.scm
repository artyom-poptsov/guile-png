;;; rotate.scm -- Image transformation methods.

;; Copyright (C) 2024 Artyom V. Poptsov <poptsov.artyom@gmail.com>
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

;; This module contains the implementation of image transformation methods.


;;; Code:


(define-module (png image-processing transform)
  #:use-module (oop goops)
  #:use-module (rnrs bytevectors)
  #:use-module (png image)
  #:use-module (png graphics pixel)
  #:use-module (png core error)
  #:export (png-image-rotate-90/ccw
            png-image-rotate-90/cw
            png-image-rotate-180
            png-image-flip-vertical
            png-image-flip-horizontal))



(define-method (png-image-rotate-90/cw (image <png-image>))
  "Rotate an @var{image} clockwise.  Return the new image."
  (let* ((image-clone     (png-image-clone image))
         (original-width  (png-image-width image))
         (original-height (png-image-height image))
         (new-width       original-height)
         (new-height      original-width))
    (png-image-width-set! image-clone new-width)
    (png-image-height-set! image-clone new-height)
    (let layer-loop ((layer-index 0))
      (if (= layer-index original-height)
          image-clone
          (begin
            (let pixel-loop ((pixel-index 0))
              (when (< pixel-index original-width)
                (png-image-pixel-set! image-clone
                                      (- new-width layer-index 1)
                                      pixel-index
                                      (png-image-pixel-ref image
                                                           pixel-index
                                                           layer-index))
                (pixel-loop (+ pixel-index 1))))
            (layer-loop (+ layer-index 1)))))))

(define-method (png-image-rotate-90/ccw (image <png-image>))
  "Rotate an @var{image} counter-clockwise.  Return the new image."
  (let* ((image-clone     (png-image-clone image))
         (original-width  (png-image-width image))
         (original-height (png-image-height image))
         (new-width       original-height)
         (new-height      original-width))
    (png-image-width-set! image-clone new-width)
    (png-image-height-set! image-clone new-height)
    (let layer-loop ((layer-index 0))
      (if (= layer-index original-height)
          image-clone
          (begin
            (let pixel-loop ((pixel-index 0))
              (when (< pixel-index original-width)
                (png-image-pixel-set! image-clone
                                      layer-index
                                      (- new-height pixel-index 1)
                                      (png-image-pixel-ref image
                                                           pixel-index
                                                           layer-index))
                (pixel-loop (+ pixel-index 1))))
            (layer-loop (+ layer-index 1)))))))

(define-method (png-image-rotate-180 (image <png-image>))
  "Rotate an @var{image} by 180 degrees.  Return a rotated new image."
  (let* ((image-clone (png-image-clone image))
         (width       (png-image-width image))
         (height      (png-image-height image)))
    (let layer-loop ((layer-index 0))
      (if (= layer-index height)
          image-clone
          (begin
            (let pixel-loop ((pixel-index 0))
              (when (< pixel-index width)
                (png-image-pixel-set! image-clone
                                      (- width pixel-index 1)
                                      (- height layer-index 1)
                                      (png-image-pixel-ref image
                                                           pixel-index
                                                           layer-index))
                (pixel-loop (+ pixel-index 1))))
            (layer-loop (+ layer-index 1)))))))

(define-method (png-image-flip-vertical (image <png-image>))
  "Do a vertical flip of an @var{image}.  Return a new flipped image."
  (let* ((image-clone (png-image-clone image))
         (width       (png-image-width image))
         (height      (png-image-height image)))
    (let layer-loop ((layer-index 0))
      (if (= layer-index height)
          image-clone
          (begin
            (let pixel-loop ((pixel-index 0))
              (when (< pixel-index width)
                (png-image-pixel-set! image-clone
                                      pixel-index
                                      (- height layer-index 1)
                                      (png-image-pixel-ref image
                                                           pixel-index
                                                           layer-index))
                (pixel-loop (+ pixel-index 1))))
            (layer-loop (+ layer-index 1)))))))

(define-method (png-image-flip-horizontal (image <png-image>))
  "Do a horizontal flip of an @var{image}.  Return a new flipped image."
  (let* ((image-clone (png-image-clone image))
         (width       (png-image-width image))
         (height      (png-image-height image)))
    (let layer-loop ((layer-index 0))
      (if (= layer-index height)
          image-clone
          (begin
            (let pixel-loop ((pixel-index 0))
              (when (< pixel-index width)
                (png-image-pixel-set! image-clone
                                      (- width pixel-index 1)
                                      layer-index
                                      (png-image-pixel-ref image
                                                           pixel-index
                                                           layer-index))
                (pixel-loop (+ pixel-index 1))))
            (layer-loop (+ layer-index 1)))))))

;;; transform.scm ends here.
