;;; graphic.scm -- Generic graphic object.

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

;; This module contains description of a generic graphic object that is the
;; root of the graphical object hierarchy.
;;
;; The inspiration for the hierarchy layout itself is an article named
;; "Applying Object-Oriented Design to Structured Graphics" by John
;; M. Vlissides and Mark A. Linton from Stanford University. [1]
;;
;; References:
;; 1. https://www.softwarepreservation.org/projects/c_plus_plus/library/interviews/graphic.pdf


;;; Code:

(define-module (png graphics graphic)
  #:use-module (rnrs bytevectors)
  #:use-module (png image)
  #:use-module (oop goops)
  #:use-module (png core common)
  #:export (<graphic>
            graphic-color
            draw!))

(define-class <graphic> ()
  (color
   #:init-keyword #:color
   #:init-thunk   (lambda () (make-bytevector 4 0))
   #:getter       graphic-color))


(define-method (%display (graphic <graphic>) (port <port>))
  (format port "#<graphic color: ~a ~a>"
          (graphic-color graphic)
          (object-address/hex-string graphic)))

(define-method (display (graphic <graphic>) (port <port>))
  (%display graphic port))

(define-method (write (graphic <graphic>) (port <port>))
  (%display graphic port))



(define-method (draw! (image <png-image>) (obj <graphic>))
  (error "Not implemented yet" image obj))

;;; graphic.scm ends here.
