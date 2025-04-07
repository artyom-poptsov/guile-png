;;; graphics.scm -- PNG graphic primitives.

;; Copyright (C) 2022-2025 Artyom V. Poptsov <poptsov.artyom@gmail.com>
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

;; This module contains re-exports everything from (png graphics ...) modules.


;;; Code:

(define-module (png graphics)
  #:use-module (png core common))

(re-export-modules (png graphics dimension)
                   (png graphics selection)
                   (png graphics graphic)
                   (png graphics point)
                   (png graphics line)
                   (png graphics multiline)
                   (png graphics hexagon)
                   (png graphics polygon)
                   (png graphics rectangle)
                   (png graphics filled-rectangle)
                   (png graphics ellipse)
                   (png graphics circle))

;;; graphics.scm ends here.
