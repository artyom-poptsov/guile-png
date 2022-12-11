;;; circle.scm -- Circle drawing algorithm implementation.

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

;; This module contains the implementation of the circle drawing algorithm.


;;; Code:

(define-module (png graphics circle)
  #:use-module (oop goops)
  #:use-module (png image)
  #:use-module (png core common)
  #:use-module (png graphics pixel)
  #:use-module (png graphics point)
  #:use-module (png graphics graphic)
  #:use-module (png graphics ellipse)
  #:export (<circle>
            circle-center
            circle-diameter
            circle-radius))


(define-class <circle> (<ellipse>)
  ;; Circle radius.
  ;;
  ;; <number>
  (radius
   #:init-keyword #:radius
   #:init-value   0
   #:getter       circle-radius))

(define-method (display (circle <circle>) (port <port>))
  (format port "#<circle cx: ~a cy: ~a r: ~a ~a>"
          (point-x (ellipse-center circle))
          (point-y (ellipse-center circle))
          (circle-radius circle)
          (object-address/hex-string circle)))


(define-method (initialize (circle <circle>) initargs)
  (next-method)
  (let ((radius   (and (memq #:radius initargs)
                       (cadr (memq #:radius initargs))))
        (diameter (and (memq #:diameter initargs)
                       (cadr (memq #:radius initargs))))
        (height   (and (memq #:height initargs)
                       (cadr (memq #:height initargs))))
        (width    (and (memq #:width initargs)
                       (cadr (memq #:width initargs)))))

    (when (or width height)
      (error "#:width and #:height are unsupported parameters"
             initargs))

    (when (and radius diameter)
      (error "Cannot use both #:radius and #:diameter"
             initargs))

    (when radius
      (let ((size (* radius)))
        (slot-set! circle 'width  size)
        (slot-set! circle 'height size)))

    (when diameter
      (slot-set! circle 'radius (/ diameter 2))
      (slot-set! circle 'width  diameter)
      (slot-set! circle 'height diameter))))


(define circle-center ellipse-center)

(define-method (circle-diameter (circle <circle>))
  (* (circle-radius circle) 2))

;;; circle.scm ends here.
