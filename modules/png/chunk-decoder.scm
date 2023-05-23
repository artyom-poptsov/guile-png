;;; chunk-decoder.scm -- PNG chunk decoder.

;; Copyright (C) 2022-2023 Artyom V. Poptsov <poptsov.artyom@gmail.com>
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

;; This module chunk decoder procedures.


;;; Code:

(define-module (png chunk-decoder)
  #:use-module (oop goops)
  #:use-module (png core chunk)
  #:use-module (png core chunk ihdr)
  #:use-module (png core chunk plte)
  #:use-module (png core chunk iend)
  #:use-module (png core chunk chrm)
  #:use-module (png core chunk text)
  #:use-module (png core chunk ztxt)
  #:use-module (png core chunk time)
  #:use-module (png core chunk iccp)
  #:use-module (png core chunk phys)
  #:use-module (png core chunk bkgd)
  #:use-module (png image)
  #:export (png-chunk-decode))


(define (make-converter proc)
  (lambda (image chunk)
    (proc chunk)))

(define %converters-to-typed
  `((IHDR . ,(make-converter png-chunk-decode-IHDR))
    (PLTE . ,(make-converter png-chunk-decode-PLTE))
    (IEND . ,(make-converter png-chunk-decode-IEND))
    (cHRM . ,(make-converter png-chunk-decode-cHRM))
    (tEXt . ,(make-converter png-chunk-decode-tEXt))
    (tEXT . ,(make-converter png-chunk-decode-tEXT))
    (zTXt . ,(make-converter png-chunk-decode-zTXt))
    (tIME . ,(make-converter png-chunk-decode-tIME))
    (iCCP . ,(make-converter png-chunk-decode-iCCP))
    (pHYs . ,(make-converter png-chunk-decode-pHYs))
    (bKGD . ,(lambda (image chunk)
               (let ((result (png-image-chunks-query image 'IHDR)))
                 (unless result
                   (error "Could not find IHDR chunk"))
                 (png-chunk-decode-bKGD chunk (car result)))))))



(define-method (png-chunk-decode image (chunk <png-chunk>))
  (let ((type (png-chunk-type chunk)))
    (if type
        (let ((converter (assoc-ref %converters-to-typed type)))
          (if converter
              (converter image chunk)
              chunk))
        (error "Unknown chunk type" type chunk))))

;;; chunk-decoder.scm ends here.
