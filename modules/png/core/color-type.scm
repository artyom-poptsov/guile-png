;;; color-type.scm -- Guile-PNG Image color type.

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

;; This module contains procedures for working with PNG image color types.


;;; Code:

(define-module (png core color-type)
  #:use-module (scheme documentation)
  #:use-module (oop goops)
  #:use-module (ice-9 hash-table)
  #:export (%color-types
            png-image-color-type->symbol
            symbol->png-image-color-type))



(define-with-docs %color-types
  "Hash table of possible PNG image color types.  Color type codes represent
sums of the following values: 1 (palette used), 2 (color used), and 4 (alpha
channel used)."
  (alist->hash-table
   '((0 . grayscale)
     (2 . rgb)
     (3 . indexed)
     (4 . grayscale+alpha)
     (6 . rgb+alpha))))

(define-method (png-image-color-type->symbol (color-type <number>))
  "Convert a PNG image @var{color-type} to a symbol."
  (hash-ref %color-types color-type))

(define-method (symbol->png-image-color-type (color-type <symbol>))
  "Lookup a corresponding color type code for a @var{color-type} symbol.  Return
the code or #f if the code is not found."
  (let loop ((lst (hash-map->list cons %color-types)))
    (if (null? lst)
        #f
        (let ((elem (car lst)))
          (if (equal? (cdr elem) color-type)
              (car elem)
              (loop (cdr lst)))))))

;;; color-type.scm ends here.
