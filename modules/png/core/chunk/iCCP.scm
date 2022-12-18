;;; iCCP.scm -- ICC Profile chunk.

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

;; ICC profile is a set of data that characterizes a color input or output
;; device, or a color space, according to standards promulgated by the
;; International Color Consortium (ICC).
;;
;; <https://en.wikipedia.org/wiki/ICC_profile>


;;; Code:

(define-module (png core chunk iCCP)
  #:use-module (srfi srfi-43)
  #:use-module (rnrs bytevectors)
  #:use-module (oop goops)
  #:use-module (zlib)
  #:use-module (ice-9 iconv)
  #:use-module (png core common)
  #:use-module (png core chunk)
  #:export (<png-chunk:iCCP>
            png-chunk:iCCP-profile-name
            png-chunk:iCCP-compression-method
            png-chunk:iCCP-profile
            png-chunk-decode-iCCP))



(define-class <png-chunk:iCCP> (<png-chunk>)
  ;; <string>
  (profile-name
   #:init-keyword #:profile-name
   #:getter       png-chunk:iCCP-profile-name)

  (compression-method
   #:init-keyword #:compression-method
   #:getter       png-chunk:iCCP-compression-method)

  (profile
   #:init-keyword #:text
   #:getter       png-chunk:iCCP-profile))


(define-method (initialize (chunk <png-chunk:iCCP>) initargs)
  (next-method)
  (slot-set! chunk 'type 'iCCP))



(define-method (%display (chunk <png-chunk:iCCP>) (port <port>))
  (let ((type (png-chunk-type-info chunk)))
    (format port "#<png-chunk:iCCP ~a: ~a ~a>"
            (list-ref type 2)
            (png-chunk:iCCP-profile-name chunk)
            (object-address/hex-string chunk))))

(define-method (display (chunk <png-chunk:iCCP>) (port <port>))
  (%display chunk port))

(define-method (write (chunk <png-chunk:iCCP>) (port <port>))
  (%display chunk port))



(define-method (png-chunk-decode-iCCP (chunk <png-chunk>))
  (let ((length (png-chunk-length chunk))
        (type   (png-chunk-type chunk))
        (data   (png-chunk-data chunk))
        (crc    (png-chunk-crc chunk)))

    (define (read-profile index profile-name compression-method)
      (let loop ((profile '())
                 (idx  index))
        (if (= idx (bytevector-length data))
            (make <png-chunk:iCCP>
              #:length  length
              #:type    type
              #:data    data
              #:crc     crc
              #:profile-name       profile-name
              #:compression-method compression-method
              #:profile            (uncompress (u8-list->bytevector profile)))
            (loop (append profile (list (bytevector-u8-ref data idx)))
                  (+ idx 1)))))

    (define (read-compression-method index profile-name)
      (read-profile (+ index 1)
                    profile-name
                    (bytevector-u8-ref data index)))

    (define (read-profile-name)
      (let loop ((profile-name '())
                 (index   0))
        (if (zero? (bytevector-u8-ref data index))
            (read-compression-method (+ index 1)
                                     (list->string profile-name))
            (loop (append profile-name
                          (list (integer->char (bytevector-u8-ref data index))))
                  (+ index 1)))))

    (read-profile-name)))

(define-method (png-chunk-encode (chunk <png-chunk:iCCP>))
  "Encode an iCCP CHUNK to a plain PNG chunk, return the new chunk."
  (let* ((profile-name (string->bytevector (png-chunk:iCCP-profile-name chunk)
                                           "US-ASCII"))
         (profile-name-length (bytevector-length profile-name))
         (compression-method (png-chunk:iCCP-compression-method chunk))
         (profile            (png-chunk:iCCP-profile chunk))
         (compressed-profile (compress (string->bytevector profile
                                                           "US-ASCII")))
         (compressed-profile-length (bytevector-length compressed-profile))
         (chunk-length       (+ profile-name-length
                                1
                                compressed-profile-length))
         (data               (make-bytevector chunk-length 0))
         (encoded-chunk      (make <png-chunk>
                               #:type 'iCCP
                               #:length chunk-length
                               #:data data)))
    (bytevector-copy! profile-name 0 data 0 profile-name-length)
    (bytevector-u8-set! data profile-name-length compression-method)
    (bytevector-copy! compressed-profile
                      0
                      data
                      (+ profile-name-length 1)
                      (compressed-profile-length))
    (png-chunk-crc-update! encoded-chunk)
    encoded-chunk))

;;; iCCP.scm ends here.
