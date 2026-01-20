;;; phys.scm -- pHYs (Physical pixel dimensions) chunk.

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

;; The pHYs chunk specifies the intended pixel size or aspect ratio for
;; display of the image.  It contains:
;;
;;   Pixels per unit, X axis: 4 bytes (unsigned integer)
;;   Pixels per unit, Y axis: 4 bytes (unsigned integer)
;;   Unit specifier:          1 byte
;;
;; The following values are legal for the unit specifier:
;;
;;   0: unit is unknown
;;   1: unit is the meter
;;
;; When the unit specifier is 0, the pHYs chunk defines pixel aspect ratio
;; only; the actual size of the pixels remains unspecified.


;;; Code:

(define-module (png core chunk phys)
  #:use-module (ice-9 hash-table)
  #:use-module (scheme documentation)
  #:use-module (srfi srfi-43)
  #:use-module (rnrs bytevectors)
  #:use-module (oop goops)
  #:use-module (png core common)
  #:use-module (png core chunk)
  #:export (<png-chunk:pHYs>
            png-chunk:pHYs-pixels-per-unit-x-axis
            png-chunk:pHYs-pixels-per-unit-y-axis
            png-chunk:pHYs-unit-specifier
            png-chunk-decode-pHYs
            pHYs-unit-specifier->symbol
            symbol->pHYs-unit-specifier
            %pHYs-unit-specifiers
            %pHYs-unit-specifiers-reverse-mapping))


;; pHYs chunk layout:
;;
;;    Pixels per unit, X axis: 4 bytes (unsigned integer)
;;    Pixels per unit, Y axis: 4 bytes (unsigned integer)
;;    Unit specifier:          1 byte
(define %pHYs-chunk-length 9)



(define-with-docs %pHYs-unit-specifiers
  "Unit specifier.  1 (METRE) is the default value.

See <https://exiftool.org/TagNames/PNG.html#PhysicalPixel>"
  (alist->hash-table
   '((0 . UNKNOWN)
     (1 . METRE))))

(define-with-docs %pHYs-unit-specifiers-reverse-mapping
  "Reverse mapping for @code{%pHYs-unit-specifiers}"
  (alist->hash-table
   (hash-map->list (lambda (key value) (cons value key))
                   %pHYs-unit-specifiers)))

(define-method (pHYs-unit-specifier->symbol (value <number>))
  (hash-ref %pHYs-unit-specifiers value))

(define-method (symbol->pHYs-unit-specifier (value <symbol>))
  (hash-ref %pHYs-unit-specifiers-reverse-mapping value))



(define-class <png-chunk:pHYs> (<png-chunk>)
  ;; <number>
  (pixels-per-unit-x-axis
   #:init-keyword #:pixels-per-unit-x-axis
   #:getter       png-chunk:pHYs-pixels-per-unit-x-axis)

  ;; <number>
  (pixels-per-unit-y-axis
   #:init-keyword #:pixels-per-unit-y-axis
   #:getter       png-chunk:pHYs-pixels-per-unit-y-axis)

  ;; Pixel units.
  ;;
  ;; See @code{%pHYs-unit-specifiers}.
  ;;
  ;; <number>
  (unit-specifier
   #:init-keyword #:unit-specifier
   #:getter       png-chunk:pHYs-unit-specifier))

(define-method (initialize (chunk <png-chunk:pHYs>) initargs)
  (next-method)
  (slot-set! chunk 'type 'pHYs)
  (slot-set! chunk 'length %pHYs-chunk-length))



(define-method (%display (chunk <png-chunk:pHYs>) (port <port>))
  (let ((type (png-chunk-type-info chunk))
        (unit-specifier (png-chunk:pHYs-unit-specifier chunk)))
    (format port "#<png-chunk:pHYs ~a: ~ax~a unit: ~a (~a) ~a>"
            (list-ref type 2)
            (png-chunk:pHYs-pixels-per-unit-x-axis chunk)
            (png-chunk:pHYs-pixels-per-unit-y-axis chunk)
            unit-specifier
            (pHYs-unit-specifier->symbol unit-specifier)
            (object-address/hex-string chunk))))

(define-method (display (chunk <png-chunk:pHYs>) (port <port>))
  (%display chunk port))

(define-method (write (chunk <png-chunk:pHYs>) (port <port>))
  (%display chunk port))

(define-method (png-chunk-decode-pHYs (chunk <png-chunk>))
  (let ((length (png-chunk-length chunk))
        (type   (png-chunk-type chunk))
        (data   (png-chunk-data chunk))
        (crc    (png-chunk-crc chunk)))
    (make <png-chunk:pHYs>
      #:length length
      #:type   type
      #:data   data
      #:crc    crc
      #:pixels-per-unit-x-axis (vector->int32 (bytevector-copy/part data 0 4))
      #:pixels-per-unit-y-axis (vector->int32 (bytevector-copy/part data 4 4))
      #:unit-specifier         (bytevector-u8-ref data 8))))

(define-method (png-chunk-encode (chunk <png-chunk:pHYs>))
  (let* ((pixels-per-unit-x (png-chunk:pHYs-pixels-per-unit-x-axis chunk))
         (pixels-per-unit-y (png-chunk:pHYs-pixels-per-unit-y-axis chunk))
         (unit-specifier    (png-chunk:pHYs-unit-specifier chunk))
         (data              (make-bytevector %pHYs-chunk-length 0))
         (encoded-chunk     (make <png-chunk>
                              #:type   'pHYs
                              #:data   data
                              #:length %pHYs-chunk-length)))
    (bytevector-copy! (int32->bytevector pixels-per-unit-x)
                      0
                      data
                      0
                      4)
    (bytevector-copy! (int32->bytevector pixels-per-unit-y)
                      0
                      data
                      4
                      4)
    (bytevector-u8-set! data 8 unit-specifier)
    (png-chunk-crc-update! encoded-chunk)
    encoded-chunk))

(define-method (png-chunk-clone (chunk <png-chunk:pHYs>))
  (make <png-chunk:pHYs>
    #:length (png-chunk-length chunk)
    #:type (png-chunk-type chunk)
    #:data (bytevector-copy (png-chunk-data chunk))
    #:crc  (png-chunk-crc chunk)
    #:pixels-per-unit-x-axis (png-chunk:pHYs-pixels-per-unit-x-axis chunk)
    #:pixels-per-unit-y-axis (png-chunk:pHYs-pixels-per-unit-y-axis chunk)
    #:unit-specifier         (png-chunk:pHYs-unit-specifier chunk)))


;;; phys.scm ends here.
