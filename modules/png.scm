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
  #:use-module (smc core log)
  #:use-module (smc fsm)
  #:use-module (png image)
  #:use-module (png fsm context)
  #:use-module (png fsm png-context)
  #:use-module (png fsm png-parser)
  #:use-module (png core chunk)
  #:use-module (png chunk-converter)
  #:export (png->scm))



(define* (png->scm port
                   #:key
                   (debug-mode? #f)
                   (raw?        #f))
  (let ((fsm (make <png-parser>)))
    (fsm-debug-mode-set! fsm debug-mode?)
    (log-use-stderr! debug-mode?)
    (let ((context (fsm-run! fsm (make <png-context>
                                   #:debug-mode? debug-mode?
                                   #:port        port))))
      (let ((image (make <png-image>
                     #:chunks (reverse (png-context-chunks context)))))
        (if raw?
            image
            (make <png-image>
              #:chunks (map (lambda (chunk)
                              (png-chunk->typed-chunk image chunk))
                            (png-image-chunks image))))))))

;;; png.scm ends here.
