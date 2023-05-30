;;; trns.scm -- Image transparency.

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

;; PNG image transparency (tRNS).


;;; Code:

(define-module (png core chunk trns)
  #:use-module (ice-9 format)
  #:use-module (srfi srfi-43)
  #:use-module (rnrs bytevectors)
  #:use-module (oop goops)
  #:use-module (png core common)
  #:use-module (png core chunk)
  #:use-module (png core chunk ihdr)
  #:use-module (png core chunk plte)
  #:export (<png-chunk:tRNS>
            png-chunk:tRNS-value
            png-chunk-decode-tRNS))



(define-class <png-chunk:tRNS> (<png-chunk>)
  ;; REQUIRED.
  ;;
  ;; <number>
  (color-type
   #:init-value   #f
   #:init-keyword #:color-type
   #:getter       png-chunk:tRNS-color-type)

  (value
   #:init-value   0
   #:init-keyword #:value
   #:getter       png-chunk:tRNS-value))

(define-method (initialize (chunk <png-chunk:tRNS>) initargs)
  (next-method)
  (slot-set! chunk 'type 'tRNS))



(define-method (%display (chunk <png-chunk:tRNS>) (port <port>))
  (let ((type (png-chunk-type-info chunk)))
    (format port "#<png-chunk:tRNS color type: ~a value: ~a ~a>"
            (png-chunk:tRNS-color-type chunk)
            (png-chunk:tRNS-value chunk)
            (object-address/hex-string chunk))))

(define-method (display (chunk <png-chunk:tRNS>) (port <port>))
  (%display chunk port))

(define-method (write (chunk <png-chunk:tRNS>) (port <port>))
  (%display chunk port))



(define-method (png-chunk-decode-tRNS (chunk <png-chunk>)
                                      (ihdr <png-chunk>))
  (let ((length (png-chunk-length chunk))
        (type   (png-chunk-type chunk))
        (data   (png-chunk-data chunk))
        (crc    (png-chunk-crc chunk))
        (color-type (png-chunk:IHDR-color-type ihdr)))
    (case color-type
      ((0)
       (make <png-chunk:tRNS>
         #:length     length
         #:type       type
         #:data       data
         #:crc        crc
         #:color-type color-type
         #:value      (vector->int32 data)))
      ((2)
       (make <png-chunk:tRNS>
         #:length     length
         #:type       type
         #:data       data
         #:crc        crc
         #:color-type color-type
         #:value      (vector (vector->int16 (bytevector-copy/part data 0 2))
                              (vector->int16 (bytevector-copy/part data 2 2))
                              (vector->int16 (bytevector-copy/part data 4 2)))))
      ((3)
       (make <png-chunk:tRNS>
         #:length     length
         #:type       type
         #:data       data
         #:crc        crc
         #:color-type color-type
         #:value      (bytevector-copy data)))
      (else
       (error "Unsupported color type for tRNS chunk" color-type)))))

(define-method (png-chunk-encode (chunk <png-chunk:tRNS>))
  (let ((color-type (png-chunk:tRNS-color-type chunk)))
    (case color-type
      ((0)
       (let* ((data (int16->bytevector (png-chunk:tRNS-value chunk)))
              (encoded-chunk (make <png-chunk>
                               #:type 'tRNS
                               #:data data
                               #:length (bytevector-length data))))
         (png-chunk-crc-update! encoded-chunk)
         encoded-chunk))
      ((2)
       (let* ((value (png-chunk:tRNS-value chunk))
              (data (make-bytevector 6))
              (r    (vector-ref value 0))
              (g    (vector-ref value 1))
              (b    (vector-ref value 2))
              (encoded-chunk (make <png-chunk>
                               #:type 'tRNS
                               #:data data
                               #:length (bytevector-length data))))
         (bytevector-copy! (int16->bytevector r) 0 data 0 2)
         (bytevector-copy! (int16->bytevector g) 0 data 2 2)
         (bytevector-copy! (int16->bytevector b) 0 data 4 2)
         (png-chunk-crc-update! encoded-chunk)
         encoded-chunk))
      ((3)
       (let* ((value (png-chunk:tRNS-value chunk))
              (data  (bytevector-copy (png-chunk:tRNS-value chunk)))
              (encoded-chunk (make <png-chunk>
                               #:type 'tRNS
                               #:data data
                               #:length (bytevector-length data))))
         (png-chunk-crc-update! encoded-chunk)
         encoded-chunk))
      (else
       (error "Unsupported color type for tRNS chunk" color-type)))))

(define-method (png-chunk-clone (chunk <png-chunk:tRNS>))
  (make <png-chunk:tRNS>
    #:length (png-chunk-length chunk)
    #:type   (png-chunk-type chunk)
    #:data   (bytevector-copy (png-chunk-data chunk))
    #:crc    (png-chunk-crc chunk)
    #:color-type (png-chunk:tRNS-color-type chunk)
    #:value  (case (png-chunk:tRNS-color-type chunk)
               ((0)
                (png-chunk:tRNS-value chunk))
               ((2)
                (vector-copy (png-chunk:tRNS-value chunk)))
               ((3)
                (bytevector-copy (png-chunk:tRNS-value chunk))))))

;;; time.scm ends here.

