;;; vpag.scm -- vpAg chunk.

;; Copyright (C) 2026 Artyom V. Poptsov <poptsov.artyom@gmail.com>
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

;; "vpAg" is a private chunk used within ImageMagick.
;;
;; See <https://github.com/tleguern/lgpng/blob/main/lgpng.h>


;;; Code:

(define-module (png core chunk vpag)
  #:use-module (srfi srfi-43)
  #:use-module (oop goops)
  #:use-module (rnrs bytevectors)
  #:use-module (png core common)
  #:use-module (png core chunk)
  #:export (<png-chunk:vpAg>
            png-chunk-decode-vpAg
            png-chunk:vpAg-width
            png-chunk:vpAg-height
            png-chunk:vpAg-unit-specifier))



(define-class <png-chunk:vpAg> (<png-chunk>)
  ;; <number>
  (width
   #:init-keyword #:width
   #:getter       png-chunk:vpAg-width)
  ;; <number>
  (height
   #:init-keyword #:height
   #:getter       png-chunk:vpAg-height)
  ;; <number>
  (unit-specifier
   #:init-keyword #:unit-specifier
   #:getter       png-chunk:vpAg-unit-specifier))

(define-method (initialize (chunk <png-chunk:vpAg>) initargs)
  (next-method)
  (slot-set! chunk 'type 'vpAg))

(define-method (%display (chunk <png-chunk:vpAg>) (port <port>))
  (let ((type (png-chunk-type-info chunk)))
    (format port "#<png-chunk:vpAg ~a width: ~a height: ~a: unit-specifier: ~a ~a>"
            (list-ref type 2)
            (png-chunk:vpAg-width chunk)
            (png-chunk:vpAg-height chunk)
            (png-chunk:vpAg-unit-specifier chunk)
            (object-address/hex-string chunk))))

(define-method (display (chunk <png-chunk:vpAg>) (port <port>))
  (%display chunk port))

(define-method (write (chunk <png-chunk:vpAg>) (port <port>))
  (%display chunk port))



(define-method (png-chunk-decode-vpAg (chunk <png-chunk>))
  (let ((length (png-chunk-length chunk))
        (type   (png-chunk-type chunk))
        (data   (png-chunk-data chunk))
        (crc    (png-chunk-crc chunk)))
    (make <png-chunk:vpAg>
      #:length      length
      #:data        data
      #:width       (vector->int32 (bytevector-copy/part data 0 4))
      #:height      (vector->int32 (bytevector-copy/part data 4 8))
      #:unit-specifier (bytevector-u8-ref data 9)
      #:type        type)))

(define-method (png-chunk-encode (chunk <png-chunk:vpAg>))
  (let* ((data (make-bytevector 9 0))
         (width  (png-chunk:vpAg-width chunk))
         (height (png-chunk:vpAg-height chunk))
         (unit-specifier (png-chunk:vpAg-unit-specifier chunk))
         (encoded-chunk (make <png-chunk>
                          #:data   data
                          #:length (bytevector-length data)
                          #:type   'vpAg)))
    (bytevector-copy! (int32->bytevector width)  0 data 0 4)
    (bytevector-copy! (int32->bytevector height) 0 data 4 4)
    (bytevector-u8-set! data 8 unit-specifier)
    (png-chunk-crc-update! encoded-chunk)
    encoded-chunk))

(define-method (png-chunk-clone (chunk <png-chunk:vpAg>))
  (make <png-chunk:vpAg>
    #:length (png-chunk-length chunk)
    #:type   (png-chunk-type   chunk)
    #:data   (bytevector-copy (png-chunk-data chunk))
    #:width  (png-chunk:vpAg-width chunk)
    #:height (png-chunk:vpAg-height chunk)
    #:unit-specifier (png-chunk:vpAg-unit-specifier chunk)
    #:crc    (png-chunk-crc    chunk)))

;;; ornt.scm ends here.
