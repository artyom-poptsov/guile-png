;;; IHDR.scm -- IHDR chunk.

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

;; PNG image header (IHDR) chunk.  The IHDR chunk must appear first.


;;; Code:

(define-module (png core chunk IHDR)
  #:use-module (srfi srfi-43)
  #:use-module (rnrs bytevectors)
  #:use-module (oop goops)
  #:use-module (png core common)
  #:use-module (png core chunk)
  #:export (<png-chunk:IHDR>
            png-chunk:IHDR-width
            png-chunk:IHDR-width-set!
            png-chunk:IHDR-height
            png-chunk:IHDR-height-set!
            png-chunk:IHDR-bit-depth
            png-chunk:IHDR-bit-depth-set!
            png-chunk:IHDR-color-type
            png-chunk:IHDR-color-type-set!
            png-chunk:IHDR-compression-method
            png-chunk:IHDR-compression-method-set!
            png-chunk:IHDR-filter-method
            png-chunk:IHDR-filter-method-set!
            png-chunk:IHDR-interlace-method
            png-chunk:IHDR-interlace-method-set!
            data:width
            data:heigth
            data:bit-depth
            data:color-type
            data:compression-method
            data:filter-method
            data:interlace-method
            png-chunk-decode-IHDR))


;; IHDR chunk layout:
;;
;;   Width:              4 bytes
;;   Height:             4 bytes
;;   Bit depth:          1 byte
;;   Color type:         1 byte
;;   Compression method: 1 byte
;;   Filter method:      1 byte
;;   Interlace method:   1 byte

(define %IHDR-chunk-length 13)


(define-class <png-chunk:IHDR> (<png-chunk>)
  ;; Image width in pixels.
  ;;
  ;; <number>
  (width
   #:init-keyword #:width
   #:getter       png-chunk:IHDR-width
   #:setter       png-chunk:IHDR-width-set!)

  ;; Image height in pixels.
  ;;
  ;; <number>
  (height
   #:init-keyword #:height
   #:getter       png-chunk:IHDR-height
   #:setter       png-chunk:IHDR-height-set!)

  ;; Single-byte integer giving the number of bits per sample or per palette
  ;; index (not per pixel).  Valid values are 1, 2, 4, 8, and 16, although not
  ;; all values are allowed for all color types.
  ;;
  ;; <number>
  (bit-depth
   #:init-keyword #:bit-depth
   #:getter       png-chunk:IHDR-bit-depth
   #:setter       png-chunk:IHDR-bit-depth-set!)

  ;; Single-byte integer that describes the interpretation of the image data.
  ;; Color type codes represent sums of the following values: 1 (palette
  ;; used), 2 (color used), and 4 (alpha channel used).  Valid values are 0,
  ;; 2, 3, 4, and 6.
  ;;
  ;; Bit depth restrictions for each color type are imposed to simplify
  ;; implementations and to prohibit combinations that do not compress well.
  ;; Decoders must support all legal combinations of bit depth and color type.
  ;; The allowed combinations are:
  ;;
  ;; Color    Allowed    Interpretation
  ;; Type    Bit Depths
  ;;
  ;; 0       1,2,4,8,16  Each pixel is a grayscale sample.
  ;;
  ;; 2       8,16        Each pixel is an R,G,B triple.
  ;;
  ;; 3       1,2,4,8     Each pixel is a palette index;
  ;;                     a PLTE chunk must appear.
  ;;
  ;; 4       8,16        Each pixel is a grayscale sample,
  ;;                     followed by an alpha sample.
  ;;
  ;; 6       8,16        Each pixel is an R,G,B triple,
  ;;                     followed by an alpha sample.
  ;;
  ;; <number>
  (color-type
   #:init-keyword #:color-type
   #:getter       png-chunk:IHDR-color-type
   #:setter       png-chunk:IHDR-color-type-set!)

  ;; Compression method is a single-byte integer that indicates the method
  ;; used to compress the image data.  At present, only compression method 0
  ;; (deflate/inflate compression with a 32K sliding window) is defined.  All
  ;; standard PNG images must be compressed with this scheme.
  ;;
  ;; <number>
  (compression-method
   #:init-keyword #:compression-method
   #:init-value   0
   #:getter       png-chunk:IHDR-compression-method
   #:setter       png-chunk:IHDR-compression-method-set!)

  ;; Filter method is a single-byte integer that indicates the preprocessing
  ;; method applied to the image data before compression.  At present, only
  ;; filter method 0 (adaptive filtering with five basic filter types) is
  ;; defined.  As with the compression method field, decoders must check this
  ;; byte and report an error if it holds an unrecognized code.
  ;;
  ;; <number>
  (filter-method
   #:init-keyword #:filter-method
   #:init-value   0
   #:getter       png-chunk:IHDR-filter-method
   #:setter       png-chunk:IHDR-filter-method-set!)

  ;; Interlace method is a single-byte integer that indicates the transmission
  ;; order of the image data.  Two values are currently defined: 0 (no
  ;; interlace) or 1 (Adam7 interlace).
  ;;
  ;; <number>
  (interlace-method
   #:init-keyword #:interlace-method
   #:init-value   0
   #:getter       png-chunk:IHDR-interlace-method
   #:setter       png-chunk:IHDR-interlace-method-set!))

(define-method (initialize (chunk <png-chunk:IHDR>) initargs)
  (next-method)
  (slot-set! chunk 'type 'IHDR))



(define-method (%display (chunk <png-chunk:IHDR>) (port <port>))
  (let ((type (png-chunk-type-info chunk)))
    (format port "#<png-chunk:IHDR ~a ~a>"
            (list-ref type 2)
            (object-address/hex-string chunk))))

(define-method (display (chunk <png-chunk:IHDR>) (port <port>))
  (%display chunk port))

(define-method (write (chunk <png-chunk:IHDR>) (port <port>))
  (%display chunk port))


(define-method (data:width (data <bytevector>))
  (vector->int32 (bytevector-copy/part data 0 4)))

(define-method (data:heigth (data <bytevector>))
  (vector->int32 (bytevector-copy/part data 4 4)))

(define-method (data:bit-depth (data <bytevector>))
  (bytevector-u8-ref data 8))

(define-method (data:color-type (data <bytevector>))
  (bytevector-u8-ref data 9))

(define-method (data:compression-method (data <bytevector>))
  (bytevector-u8-ref data 10))

(define-method (data:filter-method (data <bytevector>))
  (bytevector-u8-ref data 11))

(define-method (data:interlace-method (data <bytevector>))
  (bytevector-u8-ref data 12))


;; The methods below allow to get information about the image from the untyped
;; header chunk.

(define-method (%verify-chunk-type (chunk <png-chunk>))
  "Verify the CHUNK type.  Throw an error when the type is not IHDR."
  (let ((chunk-type (png-chunk-type chunk)))
    (unless (equal? chunk-type 'IHDR)
      (error "Wrong chunk type (expecting IHDR)" chunk chunk-type))))

(define-method (png-chunk:IHDR-width (chunk <png-chunk>))
  (%verify-chunk-type chunk)
  (data:width (png-chunk-data chunk)))

(define-method (png-chunk:IHDR-height (chunk <png-chunk>))
  (%verify-chunk-type chunk)
  (data:heigth (png-chunk-data chunk)))

(define-method (png-chunk:IHDR-color-type (chunk <png-chunk>))
  (%verify-chunk-type chunk)
  (data:color-type (png-chunk-data chunk)))

(define-method (png-chunk:IHDR-bit-depth (chunk <png-chunk>))
  (%verify-chunk-type chunk)
  (data:bit-depth (png-chunk-data chunk)))

(define-method (png-chunk:IHDR-compression-method (chunk <png-chunk>))
  (%verify-chunk-type chunk)
  (data:compression-method (png-chunk-data chunk)))

(define-method (png-chunk:IHDR-filter-method (chunk <png-chunk>))
  (%verify-chunk-type chunk)
  (data:filter-method (png-chunk-data chunk)))

(define-method (png-chunk:IHDR-interlace-method (chunk <png-chunk>))
  (%verify-chunk-type chunk)
  (data:interlace-method (png-chunk-data chunk)))



(define-method (png-chunk-decode-IHDR (chunk <png-chunk>))
  (let ((length (png-chunk-length chunk))
        (type   (png-chunk-type chunk))
        (data   (png-chunk-data chunk))
        (crc    (png-chunk-crc chunk)))
    (make <png-chunk:IHDR>
      #:length             length
      #:type               type
      #:data               data
      #:crc                crc
      #:width              (data:width data)
      #:height             (data:heigth data)
      #:bit-depth          (data:bit-depth data)
      #:color-type        (data:color-type data)
      #:compression-method (data:compression-method data)
      #:filter-method      (data:filter-method data)
      #:interlace-method   (data:interlace-method data))))

(define-method (png-chunk-encode (chunk <png-chunk:IHDR>))
  (let* ((data               (make-bytevector 13 0))
         (width              (int32->bytevector (png-chunk:IHDR-width chunk)))
         (height             (int32->bytevector (png-chunk:IHDR-height chunk)))
         (bit-depth          (png-chunk:IHDR-bit-depth chunk))
         (color-type         (png-chunk:IHDR-color-type chunk))
         (compression-method (png-chunk:IHDR-compression-method chunk))
         (filter-method      (png-chunk:IHDR-filter-method chunk))
         (interlace-method   (png-chunk:IHDR-interlace-method chunk))
         (encoded-chunk (make <png-chunk>
                          #:type   'IHDR
                          #:length %IHDR-chunk-length
                          #:data   data)))
    (bytevector-copy! width  0 data 0 4)
    (bytevector-copy! height 0 data 4 4)
    (bytevector-u8-set! data 8 bit-depth)
    (bytevector-u8-set! data 9 color-type)
    (bytevector-u8-set! data 10 compression-method)
    (bytevector-u8-set! data 11 filter-method)
    (bytevector-u8-set! data 12 interlace-method)
    (png-chunk-crc-update! encoded-chunk)
    encoded-chunk))

(define-method (png-chunk-clone (chunk <png-chunk:IHDR>))
  (make <png-chunk:IHDR>
    #:length (png-chunk-length chunk)
    #:data   (bytevector-copy (png-chunk-data chunk))
    #:crc    (png-chunk-crc    chunk)
    #:width              (png-chunk:IHDR-width chunk)
    #:height             (png-chunk:IHDR-height chunk)
    #:bit-depth  (png-chunk:IHDR-bit-depth chunk)
    #:color-type (png-chunk:IHDR-color-type chunk)
    #:compression-method (png-chunk:IHDR-compression-method chunk)
    #:filter-method      (png-chunk:IHDR-filter-method chunk)
    #:interlace-method   (png-chunk:IHDR-interlace-method chunk)))

;;; IHDR.scm ends here.
