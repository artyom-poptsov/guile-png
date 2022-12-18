;;; zTXt.scm -- Compressed text chunk.

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

;; The zTXt chunk contains textual data, just as tEXt does; however, zTXt
;; takes advantage of compression. zTXt and tEXt chunks are semantically
;; equivalent, but zTXt is recommended for storing large blocks of text.
;;
;; <https://www.rfc-editor.org/rfc/rfc2083#page-27>


;;; Code:

(define-module (png core chunk zTXt)
  #:use-module (srfi srfi-43)
  #:use-module (rnrs bytevectors)
  #:use-module (zlib)
  #:use-module (ice-9 iconv)
  #:use-module (oop goops)
  #:use-module (png core common)
  #:use-module (png core chunk)
  #:export (<png-chunk:zTXt>
            png-chunk:zTXt-keyword
            png-chunk:zTXt-compression-method
            png-chunk:zTXt-text
            png-chunk-decode-zTXt))


(define-class <png-chunk:zTXt> (<png-chunk>)
  ;; <string>
  (keyword
   #:init-keyword #:keyword
   #:getter       png-chunk:zTXt-keyword)

  (compression-method
   #:init-keyword #:compression-method
   #:getter       png-chunk:zTXt-compression-method)

  (text
   #:init-keyword #:text
   #:getter       png-chunk:zTXt-text))

(define-method (initialize (chunk <png-chunk:zTXt>) initargs)
  (next-method)
  (slot-set! chunk 'type 'zTXt))



(define-method (%display (chunk <png-chunk:zTXt>) (port <port>))
  (let ((type (png-chunk-type-info chunk)))
    (format port "#<png-chunk:zTXt ~a: ~a ~a>"
            (list-ref type 2)
            (png-chunk:zTXt-keyword chunk)
            (object-address/hex-string chunk))))

(define-method (display (chunk <png-chunk:zTXt>) (port <port>))
  (%display chunk port))

(define-method (write (chunk <png-chunk:zTXt>) (port <port>))
  (%display chunk port))



(define-method (png-chunk-decode-zTXt (chunk <png-chunk>))
  (let ((length (png-chunk-length chunk))
        (type   (png-chunk-type chunk))
        (data   (png-chunk-data chunk))
        (crc    (png-chunk-crc chunk)))

    (define (read-text index keyword compression-method)
      (let* ((compressed-text (bytevector-copy/part data
                                                    index
                                                    (- (bytevector-length data)
                                                       index)))
             (text (uncompress compressed-text)))
        (make <png-chunk:zTXt>
          #:length  length
          #:type    type
          #:data    data
          #:crc     crc
          #:keyword            keyword
          #:compression-method compression-method
          #:text               (bytevector->string text "US-ASCII"))))

    (define (read-compression-method index keyword)
      (read-text (+ index 1)
                 keyword
                 (bytevector-u8-ref data index)))

    (define (read-keyword)
      (let loop ((keyword '())
                 (index   0))
        (if (zero? (bytevector-u8-ref data index))
            (read-compression-method index (list->string keyword))
            (loop (append keyword (list (integer->char (bytevector-u8-ref data index))))
                  (+ index 1)))))

    (read-keyword)))

(define-method (png-chunk-encode (chunk <png-chunk:zTXt>))
  (let* ((keyword            (string->bytevector (png-chunk:zTXt-keyword chunk)
                                                 "US-ASCII"))
         (keyword-length     (bytevector-length keyword))
         (compression-method (png-chunk:zTXt-compression-method chunk))
         (uncompressed-text  (png-chunk:zTXt-text chunk))
         (compressed-text    (compress (string->bytevector uncompressed-text
                                                           "US-ASCII")))
         (chunk-length       (+ (bytevector-length keyword)
                                1
                                (bytevector-length compressed-text)))
         (data               (make-bytevector chunk-length 0))
         (encoded-chunk      (make <png-chunk>
                               #:type   'zTXt
                               #:length chunk-length
                               #:data   data)))
    (bytevector-copy! keyword 0 data 0 keyword-length)
    (bytevector-u8-set! data keyword-length compression-method)
    (bytevector-copy! compressed-text
                      0
                      data
                      (+ keyword-length 1)
                      (bytevector-length compressed-text))
    (png-chunk-crc-update! encoded-chunk)
    encoded-chunk))

;;; zTXt.scm ends here.
