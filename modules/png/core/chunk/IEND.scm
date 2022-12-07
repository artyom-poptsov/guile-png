;;; IEND.scm -- IEND chunk.

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

;; PNG image final chunk (IEND).  The IEND chunk must appear last.


;;; Code:

(define-module (png core chunk IEND)
  #:use-module (oop goops)
  #:use-module (rnrs bytevectors)
  #:use-module (png core common)
  #:use-module (png core chunk)
  #:export (<png-chunk:IEND>
            png-chunk-decode-IEND))



(define-class <png-chunk:IEND> (<png-chunk>))

(define-method (initialize (chunk <png-chunk:IEND>) initargs)
  (next-method)
  (slot-set! chunk 'type 'IEND))

(define-method (%display (chunk <png-chunk:IEND>) (port <port>))
  (let ((type (png-chunk-type-info chunk)))
    (format port "#<png-chunk:IEND ~a ~a>"
            (list-ref type 2)
            (object-address/hex-string chunk))))

(define-method (display (chunk <png-chunk:IEND>) (port <port>))
  (%display chunk port))

(define-method (write (chunk <png-chunk:IEND>) (port <port>))
  (%display chunk port))



(define-method (png-chunk-decode-IEND (chunk <png-chunk>))
  (let ((length (png-chunk-length chunk))
        (type   (png-chunk-type chunk))
        (data   (png-chunk-data chunk))
        (crc    (png-chunk-crc chunk)))
    (make <png-chunk:IEND>
      #:length             length
      #:type               type)))

(define-method (png-chunk-encode (chunk <png-chunk:IEND>))
  (let ((encoded-chunk (make <png-chunk>
                         #:type 'IEND)))
    (png-chunk-crc-update! encoded-chunk)
    encoded-chunk))

;;; IEND.scm ends here.
