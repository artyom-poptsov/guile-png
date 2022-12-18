;;; PLTE.scm -- PLTE chunk.

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

;; PNG image palette (PLTE) chunk.  The chunk contains from 1 to 256 palette
;; entries, , each a three-byte series of the form:
;;
;;   Red:   1 byte (0 = black, 255 = red)
;;   Green: 1 byte (0 = black, 255 = green)
;;   Blue:  1 byte (0 = black, 255 = blue)


;;; Code:

(define-module (png core chunk PLTE)
  #:use-module (ice-9 format)
  #:use-module (rnrs bytevectors)
  #:use-module (oop goops)
  #:use-module (png core common)
  #:use-module (png core chunk)
  #:export (<png-chunk:PLTE>
            png-chunk:PLTE-palette-entries-count
            png-chunk:PLTE-palette-entries
            png-chunk:PLTE-palette-entry
            palette:red
            palette:green
            palette:blue
            vector->PLTE-palette-entries
            png-chunk-decode-PLTE))



(define-class <png-chunk:PLTE> (<png-chunk>)
  ;; This field contains a palette entries, each a three-byte vector of the form:
  ;;
  ;;   #vu8(red green blue)
  ;;
  ;; <vector> of <bytevector>
  (palette-entries
   #:init-thunk   (lambda () (make-vector 0))
   #:init-keyword #:palette-entries
   #:getter       png-chunk:PLTE-palette-entries))

(define-method (initialize (chunk <png-chunk:PLTE>) initargs)
  (next-method)
  (slot-set! chunk 'type 'PLTE))



(define-method (%display (chunk <png-chunk:PLTE>) (port <port>))
  (let ((type (png-chunk-type-info chunk)))
    (format port "#<png-chunk:PLTE ~a (~a entr~:@p) ~a>"
            (list-ref type 2)
            (png-chunk:PLTE-palette-entries-count chunk)
            (object-address/hex-string chunk))))

(define-method (display (chunk <png-chunk:PLTE>) (port <port>))
  (%display chunk port))

(define-method (write (chunk <png-chunk:PLTE>) (port <port>))
  (%display chunk port))



(define-method (png-chunk:PLTE-palette-entry (chunk <png-chunk:PLTE>)
                                             (index <number>))
  (vector-ref (png-chunk:PLTE-palette-entries chunk) index))

(define-method (png-chunk:PLTE-palette-entries-count (chunk <png-chunk:PLTE>))
  (vector-length (png-chunk:PLTE-palette-entries chunk)))



(define-method (palette:red (plte <bytevector>))
  (bytevector-u8-ref plte 0))

(define-method (palette:green (plte <bytevector>))
  (bytevector-u8-ref plte 1))

(define-method (palette:blue (plte <bytevector>))
  (bytevector-u8-ref plte 2))



(define-method (vector->PLTE-palette-entries (vec <bytevector>))
  "Return a vector of palette entries, each of which is represented as a
three-byte bytevector of the following format:

  Red:   1 byte (0 = black, 255 = red)
  Green: 1 byte (0 = black, 255 = green)
  Blue:  1 byte (0 = black, 255 = blue)
"
  (let ((vlen (bytevector-length vec)))
    (let loop ((offset 0)
               (result '()))
      (if (< offset vlen)
          (loop (+ offset 3)
                (cons (u8-list->bytevector
                       (list (bytevector-u8-ref vec (+ 0 offset))
                             (bytevector-u8-ref vec (+ 1 offset))
                             (bytevector-u8-ref vec (+ 2 offset))))
                      result))
          (list->vector (reverse result))))))

(define-method (PLTE-palette-entries->vector (vec <vector>))
  "Convert a vector VEC to a PLTE chunk data."
  (let* ((data-length (vector-length vec))
         (result      (make-bytevector (* data-length 3) 0)))
    (let loop ((index 0))
      (if (< index data-length)
          (let* ((v (vector-ref vec index))
                 (r (bytevector-u8-ref v 0))
                 (g (bytevector-u8-ref v 1))
                 (b (bytevector-u8-ref v 2)))
            (bytevector-u8-set! result (+ (* index 3) 0) r)
            (bytevector-u8-set! result (+ (* index 3) 1) g)
            (bytevector-u8-set! result (+ (* index 3) 2) b)
            (loop (+ index 1)))
          result))))

(define-method (png-chunk-decode-PLTE (chunk <png-chunk>))
  (let ((length (png-chunk-length chunk))
        (type   (png-chunk-type chunk))
        (data   (png-chunk-data chunk))
        (crc    (png-chunk-crc chunk)))
    (unless (zero? (remainder (bytevector-length data) 3))
      (error "Invalid PLTE chunk: data length not divisible by 3" data))
    (make <png-chunk:PLTE>
      #:length             length
      #:type               type
      #:data               data
      #:crc                crc
      #:palette-entries    (vector->PLTE-palette-entries data))))

(define-method (png-chunk-encode (chunk <png-chunk:PLTE>))
  (let* ((entries       (png-chunk:PLTE-palette-entries chunk))
         (count         (png-chunk:PLTE-palette-entries-count chunk))
         (length        (* count 3))
         (encoded-chunk (make <png-chunk>
                          #:type   'PLTE
                          #:length length
                          #:data   (PLTE-palette-entries->vector entries))))
    (png-chunk-crc-update! encoded-chunk)
    encoded-chunk))

(define-method (png-chunk-clone (chunk <png-chunk:PLTE>))
  (make <png-chunk:PLTE>
    #:type   (png-chunk-type   chunk)
    #:data   (bytevector-copy (png-chunk-data   chunk))
    #:length (png-chunk-length chunk)
    #:crc    (png-chunk-crc    chunk)
    #:palette-entries (bytevector-copy (png-chunk:PLTE-palette-entries))))

;;; PLTE.scm ends here.
