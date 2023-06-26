;;; bkgd.scm -- bKGD chunk.

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

;; The bKGD chunk specifies a default background color to present
;; the image against.  Note that viewers are not bound to honor
;; this chunk; a viewer can choose to use a different background.
;;
;; https://www.rfc-editor.org/rfc/rfc2083#page-19


;;; Code:

(define-module (png core chunk bkgd)
  #:use-module (srfi srfi-43)
  #:use-module (oop goops)
  #:use-module (rnrs bytevectors)
  #:use-module (png core common)
  #:use-module (png core chunk)
  #:use-module (png core chunk ihdr)
  #:export (<png-chunk:bKGD>
            png-chunk:bKGD-color-type
            png-chunk:bKGD-grayscale
            png-chunk:bKGD-red
            png-chunk:bKGD-green
            png-chunk:bKGD-blue
            png-chunk:bKGD-palette-index
            png-chunk-decode-bKGD))



(define-class <png-chunk:bKGD> (<png-chunk>)
  ;; <number>
  (color-type
   #:init-keyword #:color-type
   #:init-value   0
   #:getter       png-chunk:bKGD-color-type)

  ;; <number>
  (grayscale
   #:init-keyword #:grayscale
   #:init-value   0
   #:getter       png-chunk:bKGD-grayscale)

  ;; <number>
  (red
   #:init-keyword #:red
   #:init-value   0
   #:getter       png-chunk:bKGD-red)

  ;; <number>
  (green
   #:init-keyword #:green
   #:init-value   0
   #:getter       png-chunk:bKGD-green)

  ;; <number>
  (blue
   #:init-keyword #:blue
   #:init-value   0
   #:getter       png-chunk:bKGD-blue)

  ;; Palette index of the color to be used as background.
  ;;
  ;; <number>
  (palette-index
   #:init-keyword #:palette-index
   #:init-value   0
   #:getter       png-chunk:bKGD-palette-index))


(define-method (initialize (chunk <png-chunk:bKGD>) initargs)
  (next-method)
  (slot-set! chunk 'type 'bKGD))



(define-method (%display (chunk <png-chunk:bKGD>) (port <port>))
  (let ((type (png-chunk-type-info chunk)))
    (format port "#<png-chunk:bKGD color type: ~a ~a>"
            (png-chunk:bKGD-color-type chunk)
            (object-address/hex-string chunk))))

(define-method (display (chunk <png-chunk:bKGD>) (port <port>))
  (%display chunk port))

(define-method (write (chunk <png-chunk:bKGD>) (port <port>))
  (%display chunk port))



(define-method (png-chunk-decode-bKGD (chunk <png-chunk>)
                                      (ihdr  <png-chunk>))
  "Convert a PNG chunk data to a bKGD chunk instance."
  (let ((length (png-chunk-length chunk))
        (type   (png-chunk-type chunk))
        (data   (png-chunk-data chunk))
        (crc    (png-chunk-crc chunk))
        (color-type (png-chunk:IHDR-color-type ihdr)))
    (case color-type
      ((0 4)
       (make <png-chunk:bKGD>
         #:length             length
         #:type               type
         #:data               data
         #:crc                crc
         #:color-type        color-type
         #:grayscale          (vector->int16 data)))
      ((2 6)
       (make <png-chunk:bKGD>
         #:length             length
         #:type               type
         #:data               data
         #:crc                crc
         #:color-type        color-type
         #:red                (vector->int16 (bytevector-copy/part data 0 2))
         #:green              (vector->int16 (bytevector-copy/part data 2 2))
         #:blue               (vector->int16 (bytevector-copy/part data 4 2))))
      ((3)
       (make <png-chunk:bKGD>
         #:length             length
         #:type               type
         #:data               data
         #:crc                crc
         #:color-type        color-type
         #:palette-index      (bytevector-u8-ref data 0))))))

(define-method (png-chunk-encode (chunk <png-chunk:bKGD>))
  (let ((color-type (png-chunk:bKGD-color-type chunk)))
    (case color-type
      ((0 4)
       (let* ((grayscale-value (png-chunk:bKGD-grayscale chunk))
              (data            (int16->bytevector grayscale-value))
              (encoded-chunk   (make <png-chunk>
                                 #:type   'bKGD
                                 #:data   data
                                 #:length (bytevector-length data))))
         (png-chunk-crc-update! encoded-chunk)
         encoded-chunk))
      ((2 6)
       (let* ((r (png-chunk:bKGD-red chunk))
              (g (png-chunk:bKGD-green chunk))
              (b (png-chunk:bKGD-blue chunk))
              (data (make-bytevector 6 0))
              (encoded-chunk   (make <png-chunk>
                                 #:type   'bKGD
                                 #:data   data
                                 #:length (bytevector-length data))))
         (bytevector-copy! (int16->bytevector r) 0 data 0 2)
         (bytevector-copy! (int16->bytevector g) 0 data 2 2)
         (bytevector-copy! (int16->bytevector b) 0 data 4 2)
         (png-chunk-crc-update! encoded-chunk)
         encoded-chunk))
      ((3)
       (let* ((palette-index (png-chunk:bKGD-palette-index chunk))
              (data          (make-bytevector 1 0))
              (encoded-chunk   (make <png-chunk>
                                 #:type   'bKGD
                                 #:data   data
                                 #:length (bytevector-length data))))
         (bytevector-u8-set! data 0 palette-index)
         (png-chunk-crc-update! encoded-chunk)
         encoded-chunk)))))

(define-method (png-chunk-clone (chunk <png-chunk:bKGD>))
  (make <png-chunk:bKGD>
    #:length (png-chunk-length chunk)
    #:data   (png-chunk-data   chunk)
    #:crc    (png-chunk-crc    chunk)
    #:color-type (png-chunk:bKGD-color-type chunk)
    #:grayscale  (png-chunk:bKGD-grayscale chunk)
    #:red        (png-chunk:bKGD-red chunk)
    #:green      (png-chunk:bKGD-green chunk)
    #:blue       (png-chunk:bKGD-blue chunk)
    #:palette-index (png-chunk:bKGD-palette-index chunk)))

;;; bkgd.scm ends here.
