;;; itext.scm -- International textual data (iTXt).

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

;; PNG image international textual data (iTXt).


;;; Code:

(define-module (png core chunk itxt)
  #:use-module (srfi srfi-43)
  #:use-module (rnrs bytevectors)
  #:use-module (ice-9 iconv)
  #:use-module (oop goops)
  #:use-module (png core common)
  #:use-module (png core chunk)
  #:use-module (png core chunk text)
  #:export (<png-chunk:iTXt>
            png-chunk:iTXt?
            png-chunk:iTXt-keyword
            png-chunk:iTXt-keyword-description
            png-chunk:iTXt-text
            png-chunk-decode-iTXt))




(define-class <png-chunk:iTXt> (<png-chunk:tEXt>))

(define-method (initialize (chunk <png-chunk:iTXt>) initargs)
  (next-method)
  (slot-set! chunk 'type 'iTXt))

(define (png-chunk:iTXt? x)
  (is-a? x <png-chunk:iTXt>))

(define png-chunk:iTXt-keyword png-chunk:tEXt-keyword)
(define png-chunk:iTXt-text    png-chunk:tEXt-text)



(define-method (%display (chunk <png-chunk:iTXt>) (port <port>))
  (let ((type (png-chunk-type-info chunk)))
    (format port "#<png-chunk:iTXt ~a: ~a ~a>"
            (list-ref type 2)
            (png-chunk:iTXt-keyword chunk)
            (object-address/hex-string chunk))))

(define-method (display (chunk <png-chunk:iTXt>) (port <port>))
  (%display chunk port))

(define-method (write (chunk <png-chunk:iTXt>) (port <port>))
  (%display chunk port))



(define-method (png-chunk-decode-iTXt (chunk <png-chunk>))
  (let ((length (png-chunk-length chunk))
        (type   (png-chunk-type chunk))
        (data   (png-chunk-data chunk))
        (crc    (png-chunk-crc chunk)))
    (define (read-text index keyword)
      (let loop ((text '())
                 (idx  index))
        (if (= idx (bytevector-length data))
            (make <png-chunk:iTXt>
              #:length  length
              #:type    type
              #:data    data
              #:crc     crc
              #:keyword keyword
              #:text    (utf8->string (u8-list->bytevector (reverse text))))
            (loop (cons (bytevector-u8-ref data idx) text)
                  (+ idx 1)))))

    (define (read-keyword)
      (let loop ((keyword '())
                 (index   0))
        (let ((byte (bytevector-u8-ref data index)))
          (if (zero? byte)
              (read-text (+ index 1)
                         (utf8->string (u8-list->bytevector (reverse keyword))))
              (loop (cons byte keyword)
                    (+ index 1))))))

    (read-keyword)))

(define-method (png-chunk-clone (chunk <png-chunk:iTXt>))
  (make <png-chunk:iTXt>
    #:type    (png-chunk-type   chunk)
    #:data    (bytevector-copy (png-chunk-data chunk))
    #:length  (png-chunk-length chunk)
    #:crc     (png-chunk-crc chunk)
    #:keyword (string-copy (png-chunk:iTXt-keyword chunk))
    #:text    (string-copy (png-chunk:iTXt-text chunk))))

;;; itxt.scm ends here.
