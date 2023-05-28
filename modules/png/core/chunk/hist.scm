;;; hist.scm -- Image histogram chunk.

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

;; PNG image histogram (hIST).


;;; Code:

(define-module (png core chunk hist)
  #:use-module (ice-9 format)
  #:use-module (srfi srfi-43)
  #:use-module (rnrs bytevectors)
  #:use-module (oop goops)
  #:use-module (png core common)
  #:use-module (png core chunk)
  #:export (<png-chunk:hIST>
            png-chunk:hIST-values
            png-chunk-decode-hIST))



(define-class <png-chunk:hIST> (<png-chunk>)
  ;; <list> of <number>
  (values
   #:init-value   '()
   #:init-keyword #:values
   #:getter       png-chunk:hIST-values))

(define-method (initialize (chunk <png-chunk:hIST>) initargs)
  (next-method)
  (slot-set! chunk 'type 'hIST))



(define-method (%display (chunk <png-chunk:hIST>) (port <port>))
  (let ((type (png-chunk-type-info chunk)))
    (format port "#<png-chunk:hIST ~a values ~a>"
            (png-chunk:hIST-values chunk)
            (object-address/hex-string chunk))))

(define-method (display (chunk <png-chunk:hIST>) (port <port>))
  (%display chunk port))

(define-method (write (chunk <png-chunk:hIST>) (port <port>))
  (%display chunk port))


(define-method (data:values (data <bytevector>))
  (let ((bv-length (bytevector-length data)))
    (let loop ((idx    0)
               (result '()))
      (if (= idx bv-length)
          (reverse result)
          (loop (+ idx 1)
                (cons (vector->int16 (bytevector-copy/part idx (+ idx 2)))
                      result))))))



(define-method (png-chunk-decode-hIST (chunk <png-chunk>))
  (let ((length (png-chunk-length chunk))
        (type   (png-chunk-type chunk))
        (data   (png-chunk-data chunk))
        (crc    (png-chunk-crc chunk)))
    (make <png-chunk:hIST>
      #:length length
      #:type   type
      #:data   data
      #:crc    crc
      #:values (data:values data))))

(define-method (png-chunk-encode (chunk <png-chunk:hIST>))
  (let* ((histogram-values (png-chunk:hIST-values chunk))
         (data-length      (* (length values) 2))
         (data             (make-bytevector data-length))
         (encoded-chunk    (make <png-chunk>
                             #:type   'hIST
                             #:data   data
                             #:length data-length)))
    (let loop ((values histogram-values)
               (idx    0))
      (unless (null? values)
        (let ((bv (int16->bytevector (car values))))
          (bytevector-copy! bv 0 data idx 2)
          (loop (cdr values)
                (+ idx 2)))))
    (png-chunk-crc-update! encoded-chunk)
    encoded-chunk))

(define-method (png-chunk-clone (chunk <png-chunk:hIST>))
  (make <png-chunk:hIST>
    #:length (png-chunk-length chunk)
    #:type   (png-chunk-type chunk)
    #:data   (bytevector-copy (png-chunk-data chunk))
    #:crc    (png-chunk-crc chunk)
    #:value  (list-copy (png-chunk:hIST-values chunk))))

;;; time.scm ends here.

