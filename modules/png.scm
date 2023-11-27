;;; png.scm -- GNU Guile PNG parser.

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

;; Guile-PNG is PNG format (RFC5545) [1] parser for GNU Guile.
;;
;; [1] https://tools.ietf.org/html/rfc2083


;;; Code:

(define-module (png)
  #:use-module (oop goops)
  #:use-module (png image)
  #:use-module (png fsm context)
  #:use-module (png fsm png-context)
  #:use-module (png fsm png-parser)
  #:use-module (png core chunk)
  #:use-module (png chunk-decoder)
  #:export (png->scm
            scm->png))



(define* (png->scm #:optional
                   (port (current-input-port))
                   #:key
                   (debug-mode? #f)
                   (decompress? #t)
                   (remove-filter? #t))
  "Read a PNG image from a @var{port}.  The result of the procedure is a
@code{<png-image>} instance (or <png-compressed-image> instance, when
@var{decompress?} is set to @code{#f}.)"
  (let ((fsm (make <png-parser>)))
    (log-use-stderr! debug-mode?)
    (let ((context (fsm-run! fsm (make <png-context>
                                   #:debug-mode? debug-mode?
                                   #:port        port))))
      (let ((image (make <png-compressed-image>
                     #:chunks (reverse (png-context-chunks context)))))
        (if decompress?
            (png-compressed-image-decompress image remove-filter?)
            image)))))

(define* (scm->png image
                   #:optional
                   (port (current-output-port)))
  "Convert a PNG @var{image} to binary representation, write the image object
to a @var{port}.  Return value is undefined."
  (png-image->png image port))

;;; png.scm ends here.
