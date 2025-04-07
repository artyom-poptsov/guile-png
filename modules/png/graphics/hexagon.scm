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

;; This module contains the implementation of the hexagon drawing algorithm.


;;; Code:

(define-module (png graphics hexagon)
  #:use-module (oop goops)
  #:use-module (png image)
  #:use-module (png core common)
  #:use-module (png graphics graphic)
  #:use-module (png graphics point)
  #:use-module (png graphics line)
  #:use-module (png graphics polygon)
  #:export (<hexagon>
            hexagon-center
            hexagon-diameter
            hexagon-points))


(define-class <hexagon> (<polygon>)
  ;; <point>
  (center
   #:init-value   (make <point> #:x 0 #:y 0)
   #:init-keyword #:center
   #:getter       hexagon-center)

   ;; <number>
  (diameter
   #:init-value #f
   #:init-keyword #:diameter
   #:getter       hexagon-diameter))

(define hexagon-points polygon-points)



(define-method (%display (hexagon <hexagon>) (port <port>))
  (format port "#<hexagon center: ~a diameter: ~a ~a>"
          (hexagon-center hexagon)
          (hexagon-diameter hexagon)
          (object-address/hex-string hexagon)))

(define-method (display (hexagon <hexagon>) (port <port>))
  (%display hexagon port))

(define-method (write (hexagon <hexagon>) (port <port>))
  (%display hexagon port))



;; XXX: Probably we can get this constant from somewhere else?
(define %pi 3.14159265)

(define-method (initialize (hexagon <hexagon>) initargs)
  (unless (memq #:diameter initargs)
    (error "#:diameter is required" initargs))
  (when (memq #:color initargs)
    (slot-set! hexagon
               'color
               (cadr (memq #:color initargs))))
  (let* ((diameter (cadr (memq #:diameter initargs)))
         (center   (if (memq #:center initargs)
                       (cadr (memq #:center initargs))
                       (make <point> #:x 0 #:y 0)))
         (radius (inexact->exact (floor (/ diameter 2.0))))
         (number-of-sides 6)
         (increment (/ (* 2.0 %pi) number-of-sides))
         (cx (point-x center))
         (cy (point-y center))
         (int (lambda (n) (inexact->exact (floor n))))
         (points (let loop ((sides number-of-sides)
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
    (slot-set! hexagon 'points points)))

;;; hexagon.scm ends here.
