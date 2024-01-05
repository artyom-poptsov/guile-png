;;; image-processing.scm -- Image processing procedures.

;; Copyright (C) 2022-2024 Artyom V. Poptsov <poptsov.artyom@gmail.com>
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

;; This module contains image processing procedures.


;;; Code:

(define-module (png image-processing)
  #:use-module (png core common))

(re-export-modules (png image-processing grayscale)
                   (png image-processing invert-colors)
                   (png image-processing solarize)
                   (png image-processing blur))

;;; image-processing.scm ends here.
