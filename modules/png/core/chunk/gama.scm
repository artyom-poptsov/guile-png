;;; gama.scm -- Image gamma chunk.

;; Copyright (C) 2023 Artyom V. Poptsov <poptsov.artyom@gmail.com>
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

;; PNG image gamma (gAMA).


;;; Code:

(define-module (png core chunk gama)
  #:use-module (ice-9 format)
  #:use-module (srfi srfi-43)
  #:use-module (rnrs bytevectors)
  #:use-module (oop goops)
  #:use-module (png core common)
  #:use-module (png core chunk)
  #:export (<png-chunk:gAMA>
            png-chunk:gAMA-value
            png-chunk-decode-gAMA))


(define %gAMA-chunk-length 4)



(define-class <png-chunk:gAMA> (<png-chunk>)
  (value
   #:init-value   0
   #:init-keyword #:value
   #:getter       png-chunk:gAMA-value))

(define-method (initialize (chunk <png-chunk:gAMA>) initargs)
  (next-method)
  (slot-set! chunk 'type 'gAMA)
  (slot-set! chunk 'length %gAMA-chunk-length))



(define-method (%display (chunk <png-chunk:gAMA>) (port <port>))
  (let ((type (png-chunk-type-info chunk)))
    (format port "#<png-chunk:gAMA value: ~a (~a) ~a>"
            (png-chunk:gAMA-value chunk)
            (/ (png-chunk:gAMA-value chunk) 100000)
            (object-address/hex-string chunk))))

(define-method (display (chunk <png-chunk:gAMA>) (port <port>))
  (%display chunk port))

(define-method (write (chunk <png-chunk:gAMA>) (port <port>))
  (%display chunk port))



(define-method (png-chunk-decode-gAMA (chunk <png-chunk>))
  (let ((length (png-chunk-length chunk))
        (type   (png-chunk-type chunk))
        (data   (png-chunk-data chunk))
        (crc    (png-chunk-crc chunk)))
    (make <png-chunk:gAMA>
      #:length length
      #:type   type
      #:data   data
      #:crc    crc
      #:value  (vector->int32 data))))

(define-method (png-chunk-encode (chunk <png-chunk:gAMA>))
  (let* ((value  (png-chunk:gAMA-value chunk))
         (data   (int32->bytevector value))
         (encoded-chunk (make <png-chunk>
                          #:type   'gAMA
                          #:data   data
                          #:length %gAMA-chunk-length)))
    (png-chunk-crc-update! encoded-chunk)
    encoded-chunk))

(define-method (png-chunk-clone (chunk <png-chunk:gAMA>))
  (make <png-chunk:gAMA>
    #:length (png-chunk-length chunk)
    #:type   (png-chunk-type chunk)
    #:data   (bytevector-copy (png-chunk-data chunk))
    #:crc    (png-chunk-crc chunk)
    #:value  (png-chunk:gAMA-value chunk)))

;;; time.scm ends here.

