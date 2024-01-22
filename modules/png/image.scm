;;; image.scm -- Guile-PNG Image procedures.

;; Copyright (C) 2022-2024 Artyom V. Poptsov <poptsov.artyom@gmail.com>
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

;; This module contains procedures for working with compressed and
;; decompressed images.


;;; Code:

(define-module (png image)
  #:use-module (oop goops)
  #:use-module (scheme documentation)
  #:use-module (rnrs bytevectors)
  #:use-module (rnrs io ports)
  #:use-module (ice-9 binary-ports)     ; put-bytevector
  #:use-module (ice-9 format)
  #:use-module (zlib)
  #:use-module (png core common)
  #:use-module (png core filter)
  #:use-module (png core color-type)
  #:use-module (png core chunk)
  #:use-module (png core chunk ihdr)
  #:use-module (png core chunk iend)
  #:use-module (png core chunk plte)
  #:use-module (png chunk-decoder)
  #:re-export (png-image-color-type->symbol
               symbol->png-image-color-type)
  #:export (<png-compressed-image>
            png-compressed-image?
            png-compressed-image-decompress

            <png-image>
            png-image-chunks
            png-image-chunks-query
            png-image-chunks-insert!
            png-image-clone
            png-image-width
            png-image-width-set!
            png-image-height
            png-image-height-set!
            png-image-bit-depth
            png-image-compression-method
            png-image-filter-method
            png-image-interlace-method
            png-image-palette
            png-image-color-type
            png-image-color-type/symbol
            png-image-color-type-set!
            png-image-pixel-size
            png-image-pixels
            png-image-data
            png-image-data/apply-filter
            png-image-data-set!
            png-image->png
            png-image->bytevector
            png-image-compress
            png-image-pretty-print-data

            %png-image-signature))


;; PNG image signature.
;;
;; (decimal)              137  80  78  71  13  10  26  10
;; (hexadecimal)           89  50  4e  47  0d  0a  1a  0a
;; (ASCII C notation)    \211   P   N   G  \r  \n \032 \n
;;
;; <https://www.rfc-editor.org/rfc/rfc2083#page-77>
(define %png-image-signature
  #vu8(137 80 78 71 13 10 26 10))



;; A PNG image that consists of PNG chunks.
(define-class <png-compressed-image> ()
  ;; <list> of <png-chunk>
  (chunks
   #:init-value   '()
   #:init-keyword #:chunks
   #:getter       png-image-chunks
   #:setter       png-image-chunks-set!)

  ;; Image header.
  ;;
  ;; <png-chunk>
  (header
   #:init-value   #f
   #:getter       png-image-header)

  ;; Image palette.
  ;;
  ;; <png-chunk>
  (palette
   #:init-value   #f
   #:getter       png-image-palette))

(define-method (initialize (image <png-compressed-image>) initargs)
  (next-method)
  (let ((ihdr-chunks (png-image-chunks-query image 'IHDR)))
    (when (null? ihdr-chunks)
      (error "IHDR chunk is mandatory"))
    (slot-set! image 'header (car ihdr-chunks)))

  (let ((plte-chunks (png-image-chunks-query image 'PLTE)))
    (unless (null? plte-chunks)
      (slot-set! image 'palette (car plte-chunks)))))

(define (png-compressed-image? x)
  "Check if X is a PNG image instance."
  (is-a? x <png-compressed-image>))

(define-method (png-image-clone (image <png-compressed-image>))
  "Copy a PNG IMAGE, return a new copy."
  (make <png-compressed-image>
    #:chunks (map png-chunk-clone (png-image-chunks image))))



(define-method (%display (image <png-compressed-image>) (port <port>))
  (let ((ihdr (png-image-header image)))
    (format port "#<png-compressed-image ~ax~a ~a bit color: ~a ~a>"
            (png-chunk:IHDR-width ihdr)
            (png-chunk:IHDR-height ihdr)
            (png-chunk:IHDR-bit-depth ihdr)
            (png-image-color-type->symbol (png-image-color-type image))
            (object-address/hex-string image))))

(define-method (display (image <png-compressed-image>) (port <port>))
  (%display image port))

(define-method (write (image <png-compressed-image>) (port <port>))
  (%display image port))


(define-method (png-image-chunks-query (chunks <list>) (chunk <symbol>))
  (filter (lambda (c) (equal? (png-chunk-type c) chunk)) chunks))

(define-method (png-image-chunks-query (image <png-compressed-image>) (predicate <procedure>))
  (filter predicate (png-image-chunks image)))

(define-method (png-image-chunks-query (image <png-compressed-image>) (chunk <symbol>))
  (png-image-chunks-query image (lambda (c)
                                  (equal? (png-chunk-type c) chunk))))

(define-method (png-image-chunks-query (image <png-compressed-image>) (chunk <vector>))
  (png-image-chunks-query image (lambda (c)
                                  (equal? (png-chunk-type c) chunk))))



(define-method (png-image-width (image <png-compressed-image>))
  (png-chunk:IHDR-width (png-image-header image)))

(define-method (png-image-height (image <png-compressed-image>))
  (png-chunk:IHDR-height (png-image-header image)))

(define-method (png-image-bit-depth (image <png-compressed-image>))
  (png-chunk:IHDR-bit-depth (png-image-header image)))

(define-method (png-image-color-type (image <png-compressed-image>))
  (png-chunk:IHDR-color-type (png-image-header image)))

(define-method (png-image-compression-method (image <png-compressed-image>))
  (png-chunk:IHDR-compression-method (png-image-header image)))

(define-method (png-image-filter-method (image <png-compressed-image>))
  (png-chunk:IHDR-filter-method (png-image-header image)))

(define-method (png-image-interlace-method (image <png-compressed-image>))
  (png-chunk:IHDR-interlace-method (png-image-header image)))



(define-method (png-image-data (image <png-compressed-image>) (uncompress? <boolean>))
  "Get the PNG image data as a single bytevector.  When UNCOMPRESS? option is
set to #t, the procedure returns data in uncompressed form."
  (let ((data-chunks (png-image-chunks-query image 'IDAT)))
    (let loop ((chunks data-chunks)
               (result (make-bytevector 0)))
      (if (null? chunks)
          (if uncompress?
              (uncompress result)
              result)
          (let* ((chunk         (car chunks))
                 (chunk-data    (png-chunk-data chunk))
                 (result-length (bytevector-length result))
                 (chunk-length  (bytevector-length chunk-data))
                 (new-result    (make-bytevector (+ result-length chunk-length))))
            (bytevector-copy! result 0 new-result 0 result-length)
            (bytevector-copy! chunk-data 0 new-result result-length chunk-length)
            (loop (cdr chunks) new-result))))))

(define-method (png-image-data (image <png-compressed-image>))
  "Get the decompressed PNG image data as a single bytevector."
  (png-image-data image #t))

(define-method (png-image->png (image <png-compressed-image>) (port <output-port>))
  (put-bytevector port %png-image-signature)
  (for-each (lambda (chunk)
              (png-chunk->png chunk port))
            (png-image-chunks image)))

(define-method (png-image->bytevector (image <png-compressed-image>))
  "Convert an @var{image} to a bytevector.  Return the
 bytevector."
  (call-with-values
      (lambda ()
        (open-bytevector-output-port))
    (lambda (p get-bytevector)
      (png-image->png image p)
      (get-bytevector))))



(define-class <png-image> ()
  ;; IDAT: Image data.
  ;;
  ;; <bytevector>
  (data
   #:init-thunk   (lambda () (make-bytevector 0))
   #:init-keyword #:data
   #:setter       png-image-data-set!
   #:getter       png-image-data)

  ;; <number>
  (data-chunk-size
   #:init-value   256
   #:init-keyword #:data-chunk-size
   #:getter       png-image-data-chunk-size)

  ;; <number>
  (width
   #:init-value   0
   #:init-keyword #:width
   #:getter       png-image-width
   #:setter       png-image-width-set!)

  ;; <number>
  (height
   #:init-value   0
   #:init-keyword #:height
   #:getter       png-image-height
   #:setter       png-image-height-set!)

  ;; <number>
  (bit-depth
   #:init-value   8
   #:init-keyword #:bit-depth
   #:getter       png-image-bit-depth)

  ;; Color type is a single-byte integer that describes the interpretation of
  ;; the image data.  Color type codes represent sums of the following values:
  ;; 1 (palette used), 2 (color used), and 4 (alpha channel used).  Valid
  ;; values are 0, 2, 3, 4, and 6.
  ;;
  ;; <number>
  (color-type
   #:init-value   0
   #:init-keyword #:color-type
   #:getter       png-image-color-type
   #:setter       png-image-color-type-set!)

  ;; Compression method is a single-byte integer that indicates the method
  ;; used to compress the image data.  At present, only compression method 0
  ;; (deflate/inflate compression with a 32K sliding window) is defined.
  ;;
  ;; <number>
  (compression-method
   #:init-value   0
   #:init-keyword #:compression-method
   #:getter       png-image-compression-method)

  ;; Filter method is a single-byte integer that indicates the preprocessing
  ;; method applied to the image data before compression.  At present, only
  ;; filter method 0 (adaptive filtering with five basic filter types) is
  ;; defined.
  ;;
  ;; <number>
  (filter-method
   #:init-value   0
   #:init-keyword #:filter-method
   #:getter       png-image-filter-method)


  ;; Interlace method is a single-byte integer that indicates the transmission
  ;; order of the image data.  Two values are currently defined: 0 (no
  ;; interlace) or 1 (Adam7 interlace).
  ;;
  ;; <number>
  (interlace-method
   #:init-value   0
   #:init-keyword #:interlace-method
   #:getter       png-image-interlace-method)

  ;; A vector of PNG image palette entries, each a three-byte bytevector in
  ;; the form:
  ;;
  ;;   Red:   1 byte (0 = black, 255 = red)
  ;;   Green: 1 byte (0 = black, 255 = green)
  ;;   Blue:  1 byte (0 = black, 255 = blue)
  ;;
  ;; <vector> of <bytevector>
  (palette
   #:init-value   #()
   #:init-keyword #:palette
   #:getter       png-image-palette)

  ;; Extra chunks.
  ;;
  ;; <list> of <png-chunk>
  (chunks
   #:init-value   '()
   #:init-keyword #:chunks
   #:getter       png-image-chunks
   #:setter       png-image-chunks-set!))


(define-method (initialize (image <png-image>) initargs)
  (next-method)
  (let ((data (constructor-argument #:data initargs)))
    (unless data
      (png-image-data-set! image (make-bytevector (* (png-image-width image)
                                                     (png-image-height image)
                                                     3)
                                                  0))))
  (let ((color-type (constructor-argument #:color-type initargs)))
    (cond
      ((symbol? color-type)
       (png-image-color-type-set! image
                                  (symbol->png-image-color-type color-type)))
      ((number? color-type)
       (png-image-color-type-set! image color-type))
      ((not color-type)
       (error "#:color-type must be set" color-type))
      (else
       (error "#:color-type must be either a symbol or a number" color-type)))))


(define (png-image? x)
  "Check if X is a PNG image instance."
  (is-a? x <png-image>))



(define-method (%display (image <png-image>) (port <port>))
  (format port "#<png-image ~ax~a ~a bit color: ~a ~a>"
          (png-image-width image)
          (png-image-height image)
          (png-image-bit-depth image)
          (png-image-color-type->symbol (png-image-color-type image))
          (object-address/hex-string image)))

(define-method (display (image <png-image>) (port <port>))
  (%display image port))

(define-method (write (image <png-image>) (port <port>))
  (%display image port))



(define-method (png-image-chunks-query (image <png-image>) (predicate <procedure>))
  (filter predicate (png-image-chunks image)))

(define-method (png-image-chunks-query (image <png-image>) (chunk <symbol>))
  (png-image-chunks-query image (lambda (c)
                                  (equal? (png-chunk-type c) chunk))))

(define-method (png-image-chunks-query (image <png-image>) (chunk <vector>))
  (png-image-chunks-query image (lambda (c)
                                  (equal? (png-chunk-type c) chunk))))

(define-method (png-image-chunks-insert! (image <top>)
                                         (where <symbol>)
                                         (pred? <procedure>)
                                         (new-chunk <png-chunk>))
  "Insert a @var{new-chunk} into an @var{image} chunk list.  The procedure loops
over chunk list; the position for the new chunk to be inserted is specified by
two parameters: @var{pred?} predicate is used to check whether the current
position is right, and @var{where} specifies, where the @var{new-chunk} must
be placed.  @var{where} must be either @code{'before} or @code{after} symbol.

The return values is the position where the @var{new-chunk} was inserted, or
@code{#f} when the procedure found no place for the chunk.

When @var{where} is specified as @code{after} but the chunk list for an
@var{image} is empty, the procedure always returns @code{#f} and does not do
anything.

The @var{pred?} procedure is called as follows:

@example lisp
  (pred? idx chunk)
@end example

When the chunk list is empty, the \"chunk\" is @code{#f}.

The procedure throws an error when @var{where} is a wrong symbol."
  (let loop ((chunks (png-image-chunks image))
             (idx 0)
             (result '()))
    (cond
     ((pred? idx (if (null? chunks)
                     #f
                     (car chunks)))
      (case where
        ((before)
         (png-image-chunks-set! image
                                (append result (cons new-chunk chunks)))
         idx)
        ((after)
         (if (null? chunks)
             #f
             (let ((new-chunks (append result (cons (car chunks)
                                                    (cons new-chunk
                                                          (cdr chunks))))))
               (png-image-chunks-set! image new-chunks)
               (+ idx 1))))
        (else
         (error "Unknown insert action (expecting 'before' or 'after')"
                image
                where))))
     ((null? chunks)
      #f)
     (else
      (loop (cdr chunks)
            (+ idx 1)
            (append result (list (car chunks))))))))

(define-method (png-image-chunks-insert! (image <top>)
                                         (where <symbol>)
                                         (chunk-type <symbol>)
                                         (new-chunk <png-chunk>))
  "This version of the procedure allows to specify @var{chunk-type} directly so
the @var{new-chunk} will be inserted after or before the chunk of the
specified type (depending on @var{where} value.)"
  (png-image-chunks-insert! image
                            where
                            (lambda (idx chunk)
                              (and chunk
                                   (equal? chunk-type (png-chunk-type chunk))))
                            new-chunk))

(define-method (png-image-chunks-insert! (image <top>)
                                         (where <symbol>)
                                         (position <number>)
                                         (new-chunk <png-chunk>))
  "This version of the procedure allows to specify the position of the
@var{new-chunk} directly, so it will be inserted after or before the chunk of
the specified type (depending on @var{where} value.)"
  (png-image-chunks-insert! image
                            where
                            (lambda (idx chunk)
                              (= idx position))
                            new-chunk))



(define-method (png-image-color-type->pixel-size (color-type <number>))
  (case color-type
    ((0 3) 1)
    ((2)   3)
    ((4)   2)
    ((6)   4)))

(define-method (png-image-pixel-size (image <png-image>))
  (png-image-color-type->pixel-size (png-image-color-type image)))

(define-method (png-image-color-type/symbol (image <png-image>))
  "Get the @var{image} color type as a symbol."
  (png-image-color-type->symbol (png-image-color-type image)))

(define-method (png-image-pixels (image <png-image>))
  (* (png-image-width image) (png-image-height image)))

;; 4.1.3. IDAT Image data
;;
;; The IDAT chunk contains the actual image data.  To create this
;; data:
;;
;; * Begin with image scanlines represented as described in Image layout
;;   (Section 2.3); the layout and total size of this raw data are determined by
;;   the fields of IHDR.
;;
;; * Filter the image data according to the filtering method specified by the
;;   IHDR chunk.  (Note that with filter method 0, the only one currently
;;   defined, this implies prepending a filter type byte to each scanline.)
;;
;; * Compress the filtered data using the compression method
;;   specified by the IHDR chunk.
;;
;; <https://www.rfc-editor.org/rfc/rfc2083#page-18>
(define-method (png-image-data/remove-filter (image <png-compressed-image>)
                                             (uncompressed-data <bytevector>))
  "This method removes filter data from each scanline of IMAGE-DATA.  Return a
new bytevector with image data with filter type bytes removed."
  (let* ((width             (png-image-width image))
         (height            (png-image-height image))
         (color-type        (png-image-color-type image))
         (bytes-per-pixel   (png-image-color-type->pixel-size color-type)))
    (png-filter-remove uncompressed-data
                       #:scanline-length (* width bytes-per-pixel)
                       #:bytes-per-pixel bytes-per-pixel
                       #:image-height    height
                       #:image-width     width)))

(define-method (png-image-data/apply-filter (image <png-image>))
  (let* ((image-data        (png-image-data image))
         (width             (png-image-width image))
         (height            (png-image-height image))
         (color-type        (png-image-color-type image))
         (pixel-size        (png-image-color-type->pixel-size color-type))
         (scanline-length   (* width pixel-size)))
    (png-filter-apply image-data
                      #:scanline-length scanline-length
                      #:bytes-per-pixel pixel-size
                      #:color-type color-type
                      #:image-height height
                      #:image-width width)))

(define-method (png-image-pretty-print-data (image <png-image>) (port <port>))
  (let* ((width             (png-image-width image))
         (height            (png-image-height image))
         (color-type        (png-image-color-type image))
         (pixel-size        (png-image-color-type->pixel-size color-type))
         (scanline-length   (* width pixel-size))
         (image-data        (png-image-data image))
         (image-data-length (bytevector-length image-data)))

    (define (print-pixel offset)
      (let loop-over-pixel ((index 0))
        (unless (= index pixel-size)
          (format port "~3a " (bytevector-u8-ref image-data (+ offset index)))
          (loop-over-pixel (+ index 1))))
      (format port " "))

    (let loop-over-rows ((row-index 0))
      (unless (= row-index height)
        (let loop-over-scanline ((pixel-index 0))
          (unless (= pixel-index scanline-length)
            (print-pixel (+ (* row-index scanline-length) pixel-index))
            (loop-over-scanline (+ pixel-index pixel-size))))
        (newline port)
        (loop-over-rows (+ row-index 1))))))

(define-method (png-image-pretty-print-data (image <png-compressed-image>) (port <port>))
  (let* ((width             (png-image-width image))
         (height            (png-image-height image))
         (color-type        (png-image-color-type image))
         (pixel-size        (png-image-color-type->pixel-size color-type))
         (scanline-length   (+ (* width pixel-size) 1))
         (image-data        (png-image-data image))
         (image-data-length (bytevector-length image-data)))

    (define (print-pixel offset)
      (let loop-over-pixel ((index 0))
        (unless (= index pixel-size)
          (format port "~3a " (bytevector-u8-ref image-data (+ offset index)))
          (loop-over-pixel (+ index 1))))
      (format port " "))

    (let loop-over-rows ((row-index 0))
      (unless (= row-index height)
        (format port "~3a  " (bytevector-u8-ref image-data (* row-index scanline-length)))
        (let loop-over-scanline ((pixel-index 1))
          (unless (= pixel-index scanline-length)
            (print-pixel (+ (* row-index scanline-length) pixel-index))
            (loop-over-scanline (+ pixel-index pixel-size))))
        (newline port)
        (loop-over-rows (+ row-index 1))))))

(define-method (png-image-pretty-print-data (image <png-image>))
  (png-image-pretty-print-data image (current-error-port)))

(define-method (png-image-pretty-print-data (image <png-compressed-image>))
  (png-image-pretty-print-data image (current-error-port)))

(define-method (png-compressed-image-decompress (image <png-compressed-image>)
                                                (remove-filter? <boolean>))
  "Decompress an IMAGE, return a new <png-image> instance with uncompressed
data."
  (let ((chunks (map (lambda (chunk)
                       (png-chunk-decode image chunk))
                     (map png-chunk-clone (png-image-chunks image)))))
    (make <png-image>
      #:chunks             chunks
      #:width              (png-image-width image)
      #:height             (png-image-height image)
      #:bit-depth          (png-image-bit-depth image)
      #:color-type         (png-image-color-type image)
      #:compression-method (png-image-compression-method image)
      #:filter-method      (png-image-filter-method image)
      #:interlace-method   (png-image-interlace-method image)
      #:palette (let ((plte-chunks (png-image-chunks-query chunks 'PLTE)))
                  (if (null? plte-chunks)
                      #()
                      (png-chunk:PLTE-palette-entries (car plte-chunks))))
      #:data (if remove-filter?
                 (png-image-data/remove-filter image
                                               (png-image-data image))
                 (png-image-data image))
      #:data-chunk-size (let ((idat (car (png-image-chunks-query image 'IDAT))))
                          (png-chunk-length idat)))))

(define-method (png-compressed-image-decompress (image <png-compressed-image>))
  (png-compressed-image-decompress image #t))

(define* (png-image-compress image
                             #:key
                             (data-chunk-size #f))
  (let* ((data            (png-image-data image))
         (data/filtered   (png-image-data/apply-filter image))
         ;; (d (format (current-error-port) "** data/filtered: ~a~%" data/filtered))
         (compressed-data (compress data/filtered))
         (chunk-size      (or data-chunk-size
                              (png-image-data-chunk-size image)))
         (segments        (map (lambda (data)
                                 (let ((chunk (make <png-chunk>
                                                #:length (bytevector-length data)
                                                #:type   'IDAT
                                                #:data   data)))
                                   (png-chunk-crc-update! chunk)
                                   chunk))
                               (bytevector-split compressed-data chunk-size)))
         (header (make <png-chunk:IHDR>
                   #:width              (png-image-width image)
                   #:height             (png-image-height image)
                   #:bit-depth          (png-image-bit-depth image)
                   #:color-type         (png-image-color-type image)
                   #:compression-method (png-image-compression-method image)
                   #:filter-method      (png-image-filter-method image)
                   #:interlace-method   (png-image-interlace-method image)))
         (extra-chunks (png-image-chunks-query image
                                               (lambda (chunk)
                                                 (and (not (equal? (png-chunk-type chunk)
                                                                   'IHDR))
                                                      (not (equal? (png-chunk-type chunk)
                                                                   'PLTE))
                                                      (not (equal? (png-chunk-type chunk)
                                                                   'IDAT))
                                                      (not (equal? (png-chunk-type chunk)
                                                                   'IEND))))))
         (iend-chunk (make <png-chunk:IEND>)))
    (png-chunk-crc-update! iend-chunk)
    (png-chunk-crc-update! header)
    (if (= (png-image-color-type image) 3)
        (make <png-compressed-image>
          #:chunks (cons (png-chunk-encode header)
                         (cons (png-chunk-encode
                                (make <png-chunk:PLTE>
                                  #:palette-entries (png-image-palette image)))
                               (append extra-chunks (append segments (list iend-chunk))))))
        (make <png-compressed-image>
          #:chunks (cons (png-chunk-encode header)
                         (append extra-chunks (append segments (list iend-chunk))))))))

(define-method (png-image->png (image <png-image>) (port <output-port>))
  (let ((compressed-image (png-image-compress image)))
    (png-image->png compressed-image port)))

(define-method (png-image->png (image <png-image>))
  (png-image->png image (current-output-port)))

(define-method (png-image->bytevector (image <png-image>))
  "Compress an @var{image} and convert it to a bytevector.  Return the
 bytevector."
  (png-image->bytevector (png-image-compress image)))

(define-method (png-image-clone (image <png-image>))
  "Copy a PNG IMAGE, return a new copy."
  (let ((chunks (map png-chunk-clone (png-image-chunks image))))
    (make <png-image>
      #:chunks             chunks
      #:data-chunk-size    (png-image-data-chunk-size image)
      #:width              (png-image-width image)
      #:height             (png-image-height image)
      #:bit-depth          (png-image-bit-depth image)
      #:color-type         (png-image-color-type image)
      #:compression-method (png-image-compression-method image)
      #:filter-method      (png-image-filter-method image)
      #:interlace-method   (png-image-interlace-method image)
      #:palette            (list->vector
                            (map bytevector-copy
                                 (vector->list (png-image-palette image))))
      #:data   (bytevector-copy (png-image-data image)))))


;; image.scm ends here.
