;;; tEXt.scm -- Text chunk.

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

;; PNG image textual data (tEXt).


;;; Code:

(define-module (png core chunk tEXt)
  #:use-module (srfi srfi-43)
  #:use-module (rnrs bytevectors)
  #:use-module (ice-9 iconv)
  #:use-module (oop goops)
  #:use-module (png core common)
  #:use-module (png core chunk)
  #:export (<png-chunk:tEXt>
            %tEXt-predefined-keywords
            png-chunk:tEXt?
            png-chunk:tEXt-keyword
            png-chunk:tEXt-keyword-description
            png-chunk:tEXt-text
            png-chunk-decode-tEXt))


;; RFC 2083, "Textual data"
;;   <https://www.rfc-editor.org/rfc/rfc2083#page-24>
;;
;; The keyword indicates the type of information represented by
;; the text string.  The following keywords are predefined and
;; should be used where appropriate.
(define %tEXt-predefined-keywords
  '(("Title"         . "Short (one line) title or caption for image")
    ("Author"        . "Name of image's creator")
    ("Description"   . "Description of image (possibly long)")
    ("Copyright"     . "Copyright notice")
    ("Creation Time" . "Time of original image creation")
    ("Software"      . "Software used to create the image")
    ("Disclaimer"    . "Legal disclaimer")
    ("Warning"       . "Warning of nature of content")
    ("Source"        . "Device used to create the image")
    ("Comment"       . "Miscellaneous comment; conversion from GIF comment")))



(define-class <png-chunk:tEXt> (<png-chunk>)
  ;; <string>
  (keyword
   #:init-value   ""
   #:init-keyword #:keyword
   #:getter       png-chunk:tEXt-keyword)

  ;; <string>
  (text
   #:init-value   ""
   #:init-keyword #:text
   #:getter       png-chunk:tEXt-text))

(define-method (initialize (chunk <png-chunk:tEXt>) initargs)
  (next-method)
  (slot-set! chunk 'type 'tEXt))

(define (png-chunk:tEXt? x)
  (is-a? x <png-chunk:tEXt>))

(define-method (png-chunk:tEXt-keyword-description (chunk <png-chunk:tEXt>))
  (assoc-ref %tEXt-predefined-keywords
             (png-chunk:tEXt-keyword chunk)))



(define-method (%display (chunk <png-chunk:tEXt>) (port <port>))
  (let ((type (png-chunk-type-info chunk)))
    (format port "#<png-chunk:tEXt ~a: ~a ~a ~a>"
            (list-ref type 2)
            (png-chunk:tEXt-keyword chunk)
            (let ((description (png-chunk:tEXt-keyword-description chunk)))
              (if description
                  (format #f "(~a)" description)
                  ""))
            (object-address/hex-string chunk))))

(define-method (display (chunk <png-chunk:tEXt>) (port <port>))
  (%display chunk port))



(define-method (png-chunk-decode-tEXt (chunk <png-chunk>))
  (let ((length (png-chunk-length chunk))
        (type   (png-chunk-type chunk))
        (data   (png-chunk-data chunk))
        (crc    (png-chunk-crc chunk)))
    (define (read-text index keyword)
      (let loop ((text '())
                 (idx  index))
        (if (= idx (bytevector-length data))
            (make <png-chunk:tEXt>
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

(define-method (png-chunk-encode (chunk <png-chunk:tEXt>))
  (let* ((keyword        (string->bytevector (png-chunk:tEXt-keyword chunk)
                                             "US-ASCII"))
         (keyword-length (bytevector-length keyword))
         (text           (string->bytevector (png-chunk:tEXt-text chunk)
                                             "US-ASCII"))
         (text-length    (bytevector-length text))
         (chunk-length   (+ keyword-length
                            1
                            text-length))
         (data           (make-bytevector chunk-length 0))
         (encoded-chunk  (make <png-chunk>
                           #:type   'tEXt
                           #:data   data
                           #:length chunk-length)))
    (bytevector-copy! keyword 0 data 0 keyword-length)
    (bytevector-copy! text
                      0
                      data
                      (+ keyword-length 1)
                      text-length)
    (png-chunk-crc-update! encoded-chunk)
    encoded-chunk))

(define-method (png-chunk-clone (chunk <png-chunk:tEXt>))
  (make <png-chunk:tEXt>
    #:type    (png-chunk-type   chunk)
    #:data    (bytevector-copy (png-chunk-data chunk))
    #:length  (png-chunk-length chunk)
    #:crc     (png-chunk-crc chunk)
    #:keyword (string-copy (png-chunk:tEXt-keyword chunk))
    #:text    (string-copy (png-chunk:tEXt-text chunk))))

;;; tEXt.scm ends here.
