;;; hexagon.scm -- Hexagon drawing algorithm implementation.

;; Copyright (C) 2025 Artyom V. Poptsov <poptsov.artyom@gmail.com>
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

;; This module contains the implementation of the regular polygon[1] drawing
;; algorithm.
;;
;; 1. https://en.wikipedia.org/wiki/Regular_polygon


;;; Code:

(define-module (png graphics regular-polygon)
  #:use-module (oop goops)
  #:use-module (png image)
  #:use-module (png core common)
  #:use-module (png graphics graphic)
  #:use-module (png graphics point)
  #:use-module (png graphics line)
  #:use-module (png graphics polygon)
  #:export (<regular-polygon>
            regular-polygon-center
            regular-polygon-diameter
            regular-polygon-points
            regular-polygon-calculate-points))


(define-class <regular-polygon> (<polygon>)
  ;; <point>
  (center
   #:init-value   (make <point> #:x 0 #:y 0)
   #:init-keyword #:center
   #:getter       regular-polygon-center)

  (sides
   #:init-value   3
   #:init-keyword #:sides
   #:getter       regular-polygon-sides)

   ;; <number>
  (diameter
   #:init-value #f
   #:init-keyword #:diameter
   #:getter       regular-polygon-diameter))

(define regular-polygon-points polygon-points)



(define-method (%display (regular-polygon <regular-polygon>) (port <port>))
  (format port "#<regular-polygon center: ~a diameter: ~a sides: ~a ~a>"
          (regular-polygon-center regular-polygon)
          (regular-polygon-diameter regular-polygon)
          (regular-polygon-sides regular-polygon)
          (object-address/hex-string regular-polygon)))

(define-method (display (regular-polygon <regular-polygon>) (port <port>))
  (%display regular-polygon port))

(define-method (write (regular-polygon <regular-polygon>) (port <port>))
  (%display regular-polygon port))



;; XXX: Probably we can get this constant from somewhere else?
(define %pi 3.14159265)

(define* (regular-polygon-calculate-points #:key
                                           diameter
                                           center
                                           sides)
  (let* ((radius (inexact->exact (floor (/ diameter 2.0))))
         (increment (/ (* 2.0 %pi) sides))
         (cx (point-x center))
         (cy (point-y center))
         (int (lambda (n) (inexact->exact (floor n))))
         (points (let loop ((sides sides)
                            (angle 0)
                            (points '()))
                   (if (zero? sides)
                       points
                       (loop (- sides 1)
                             (+ angle increment)
                             (cons (make <point>
                                     #:x (int (+ (* radius (cos angle))
                                                 cx))
                                     #:y (int (+ (* radius (sin angle))
                                                 cy)))
                                   points))))))
    points))


(define-method (initialize (regular-polygon <regular-polygon>) initargs)
  (unless (memq #:diameter initargs)
    (error "#:diameter is required" initargs))
  (when (memq #:color initargs)
    (slot-set! regular-polygon
               'color
               (cadr (memq #:color initargs))))
  (let* ((diameter (cadr (memq #:diameter initargs)))
         (center   (if (memq #:center initargs)
                       (cadr (memq #:center initargs))
                       (make <point> #:x 0 #:y 0)))
         (sides (if (memq #:sides initargs)
                    (cadr (memq #:sides initargs))
                    (slot-ref regular-polygon 'sides)))
         (points (regular-polygon-calculate-points #:diameter diameter
                                                   #:center center
                                                   #:sides sides)))
    (slot-set! regular-polygon 'center center)
    (slot-set! regular-polygon 'diameter diameter)
    (slot-set! regular-polygon 'points points)))

;;; regular-polygon.scm ends here.
