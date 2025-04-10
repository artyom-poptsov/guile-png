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
  #:use-module (png graphics regular-polygon)
  #:export (<hexagon>
            hexagon-center
            hexagon-diameter
            hexagon-points))


(define-class <hexagon> (<regular-polygon>))

(define hexagon-center regular-polygon-center)
(define hexagon-points regular-polygon-points)
(define hexagon-diameter regular-polygon-diameter)



(define-method (%display (hexagon <hexagon>) (port <port>))
  (format port "#<hexagon center: ~a diameter: ~a ~a>"
          (hexagon-center hexagon)
          (hexagon-diameter hexagon)
          (object-address/hex-string hexagon)))

(define-method (display (hexagon <hexagon>) (port <port>))
  (%display hexagon port))

(define-method (write (hexagon <hexagon>) (port <port>))
  (%display hexagon port))



(define-method (initialize (hexagon <hexagon>) initargs)
  (slot-set! hexagon 'sides 6)
  (next-method))

;;; hexagon.scm ends here.
