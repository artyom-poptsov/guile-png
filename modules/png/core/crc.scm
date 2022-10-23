;;; png.scm -- GNU Guile PNG parser.

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

;; This code is the adaptation of the RFC2083 Sample CRC Code[1].
;;
;; [1] https://www.rfc-editor.org/rfc/rfc2083#section-15


;;; Code:

(define-module (png core crc)
  #:use-module (oop goops)
  #:use-module (srfi srfi-1)            ; iota
  #:use-module (rnrs bytevectors)
  #:export (make-crc-table
            crc-update
            crc))


(define (make-crc-table)
  "Make the table for a fast CRC."
  (list->vector (map (lambda (n)
                       (let loop ((k 0)
                                  (c n))
                         (if (= k 8)
                             c
                             (if (zero? (logand c 1))
                                 (loop (+ k 1) (ash c -1))
                                 (loop (+ k 1)
                                       (logxor #xEDB88320
                                               (ash c -1)))))))
                     (iota 256 0 1))))


(define %crc-table (make-crc-table))


(define-method (crc-update (buf <bytevector>))
  "Update a running CRC with the bytes buf[0..len-1]."
  (let ((bv-length (bytevector-length buf)))
    (let loop ((index 0)
               (c     #xFFFFFFFF))
      (if (= index bv-length)
          c
          (let ((byte (bytevector-u8-ref buf index)))
            (loop (+ index 1)
                  (logxor (vector-ref %crc-table
                                      (logand (logxor c byte) #xFF))
                          (ash c -8))))))))

(define-method (crc (buf <bytevector>))
  (logxor (crc-update buf) #xFFFFFFFF))

;;; crc.scm
