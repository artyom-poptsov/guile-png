;;; ornt.scm -- orNT chunk.

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

;; "orNT" is a private chunk used within ImageMagick to specify the image
;; orientation.
;;
;; See <https://github.com/tleguern/lgpng/blob/main/lgpng.h>


;;; Code:

(define-module (png core chunk ornt)
  #:use-module (srfi srfi-43)
  #:use-module (oop goops)
  #:use-module (rnrs bytevectors)
  #:use-module (png core common)
  #:use-module (png core chunk)
  #:export (<png-chunk:orNT>
            png-chunk-decode-orNT
            png-chunk:orNT-orientation))



(define-class <png-chunk:orNT> (<png-chunk>)
  ;; <number>
  (oriantation
   #:init-keyword #:orientation
   #:getter       png-chunk:orNT-orientation))

(define-method (initialize (chunk <png-chunk:orNT>) initargs)
  (next-method)
  (slot-set! chunk 'type 'orNT))

(define-method (%display (chunk <png-chunk:orNT>) (port <port>))
  (let ((type (png-chunk-type-info chunk)))
    (format port "#<png-chunk:orNT ~a orientation: ~a ~a>"
            (list-ref type 2)
            (png-chunk:orNT-orientation chunk)
            (object-address/hex-string chunk))))

(define-method (display (chunk <png-chunk:orNT>) (port <port>))
  (%display chunk port))

(define-method (write (chunk <png-chunk:orNT>) (port <port>))
  (%display chunk port))



(define-method (data:orientation (vec <bytevector>))
  (bytevector-u8-ref vec 0))

(define-method (png-chunk-decode-orNT (chunk <png-chunk>))
  (let ((length (png-chunk-length chunk))
        (type   (png-chunk-type chunk))
        (data   (png-chunk-data chunk))
        (crc    (png-chunk-crc chunk)))
    (make <png-chunk:orNT>
      #:length      length
      #:data        data
      #:type        type
      #:orientation (data:orientation data))))

(define-method (png-chunk-encode (chunk <png-chunk:orNT>))
  (let* ((orientation (png-chunk:orNT-orientation chunk))
         (data (make-bytevector 1 0))
         (encoded-chunk (make <png-chunk>
                          #:data data
                          #:length (bytevector-length data)
                          #:type 'orNT)))
    (bytevector-u8-set! data 0 orientation)
    (png-chunk-crc-update! encoded-chunk)
    encoded-chunk))

(define-method (png-chunk-clone (chunk <png-chunk:orNT>))
  (make <png-chunk:orNT>
    #:length (png-chunk-length chunk)
    #:type   (png-chunk-type   chunk)
    #:data   (bytevector-copy (png-chunk-data chunk))
    #:crc    (png-chunk-crc    chunk)))

;;; ornt.scm ends here.
