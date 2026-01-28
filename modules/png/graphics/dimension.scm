;;; dimension.scm -- Dimension class implementation.

;; Copyright (C) 2023-2026 Artyom V. Poptsov <poptsov.artyom@gmail.com>
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

;; This module contains the implementation of the <dimension> class that
;; describes a dimension of a 2D object.


;;; Code:

(define-module (png graphics dimension)
  #:use-module (oop goops)
  #:use-module (png core common)
  #:export (<dimension>
            dimension?
            dimension-width
            dimension-height))



;; A dimension of a 2D object.
(define-class <dimension> ()
  ;; Width.
  ;;
  ;; <number>
  (width
   #:init-value   0
   #:init-keyword #:width
   #:getter       dimension-width)

  ;; Height.
  ;;
  ;; <number>
  (height
   #:init-value   0
   #:init-keyword #:height
   #:getter       dimension-height))


(define-method (%display (dimension <dimension>) (port <port>))
  (format port "#<dimension ~ax~a ~a>"
          (dimension-width dimension)
          (dimension-height dimension)
          (object-address/hex-string dimension)))

(define-method (display (dimension <dimension>) (port <port>))
  (%display dimension port))

(define-method (write (dimension <dimension>) (port <port>))
  (%display dimension port))



(define (dimension? x)
  "Check if X is a <dimension> instance."
  (is-a? x <dimension>))

;;; dimension.scm ends here.
