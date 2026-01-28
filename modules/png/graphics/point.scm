;;; point.scm -- Dimension class implementation.

;; Copyright (C) 2022-2026 Artyom V. Poptsov <poptsov.artyom@gmail.com>
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

;; This module contains the implementation of the <point> class that describes
;; a point in 2D space.


;;; Code:

(define-module (png graphics point)
  #:use-module (oop goops)
  #:use-module (png image)
  #:use-module (png core common)
  #:use-module (png graphics pixel)
  #:use-module (png graphics graphic)
  #:export (<point>
            point-x
            point-y))


;; A point in 2D space.
(define-class <point> (<graphic>)
  ;; A coordinate on X axis.
  ;;
  ;; <number>
  (x
   #:init-value   0
   #:init-keyword #:x
   #:getter       point-x)

  ;; A coordinate on Y axis.
  ;;
  ;; <number>
  (y
   #:init-value   0
   #:init-keyword #:y
   #:getter       point-y))


(define-method (%display (point <point>) (port <port>))
  (format port "#<point x: ~a y: ~a ~a>"
          (point-x point)
          (point-y point)
          (object-address/hex-string point)))

(define-method (display (point <point>) (port <port>))
  (%display point port))

(define-method (write (point <point>) (port <port>))
  (%display point port))



(define-method (draw! (image <png-image>) (point <point>))
  "Draw a @var{point} (a pixel) on an @var{image} instance."
  (png-image-pixel-set! image
                        (point-x point)
                        (point-y point)
                        (graphic-color point)))

;;; point.scm ends here.
