;;; common.scm -- Common Guile-PNG procedures.

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

;; This module contains common procedures that are used in different parts of
;; Guile-PNG.


;;; Code:

(define-module (png core common)
  #:use-module (oop goops)
  #:use-module (rnrs bytevectors)
  #:export (object-address/hex-string
            vector->int32
            vector->int16
            int16->bytevector
            int32->bytevector
            bytevector-copy/part
            bytevector-copy/part!
            bytevector-split
            constructor-argument
            re-export-modules))


(define (object-address/hex-string object)
  (number->string (object-address object) 16))


(define-method (vector->int32 (v <bytevector>))
  (logior (ash (bytevector-u8-ref v 0) 24)
          (ash (bytevector-u8-ref v 1) 16)
          (ash (bytevector-u8-ref v 2) 8)
          (bytevector-u8-ref v 3)))

(define-method (vector->int16 (v <bytevector>))
  (logior (ash (bytevector-u8-ref v 0) 8)
          (bytevector-u8-ref v 1)))

(define-method (int32->bytevector (number <number>))
  "Convert a NUMBER to a byte vector."
  (u8-list->bytevector
   (list (ash (logand number #xFF000000) -24)
         (ash (logand number #x00FF0000) -16)
         (ash (logand number #x0000FF00) -8)
         (logand number #x000000FF))))

(define-method (int16->bytevector (number <number>))
  "Convert a NUMBER to a byte vector."
  (u8-list->bytevector
   (list (ash (logand number #xFF00) -8)
         (logand number #xFF))))

(define-method (bytevector-copy/part bv source-start len)
  (let ((result (make-bytevector len)))
    (bytevector-copy! bv source-start result 0 len)
    result))

(define* (bytevector-copy/part! bv
                                result
                                #:key
                                (source-offset 0)
                                (source-length (bytevector-length bv))
                                (target-offset 0))
  (bytevector-copy! bv source-offset result target-offset source-length))

(define-method (bytevector-split (bv <bytevector>) (chunk-size <number>))
  "Split a bytevector BV into parts of CHUNK-SIZE.  Return a list of
bytevectors."
  (let ((bv-length (bytevector-length bv)))
    (define (calculate-length index)
      (let ((n (- bv-length index)))
        (if (> n chunk-size)
            chunk-size
            n)))
    (let loop ((index  0)
               (result '()))
      (if (< index bv-length)
          (loop (+ index chunk-size)
                (cons (bytevector-copy/part bv
                                            index
                                            (calculate-length index))
                      result))
          (reverse result)))))

(define (constructor-argument keyword initargs)
  (and (memq keyword initargs)
       (cadr (memq keyword initargs))))



;; This macro is taken from Guile-JSON.
(define-syntax re-export-modules
  (syntax-rules ()
    ((_ (mod ...) ...)
     (begin
       (module-use! (module-public-interface (current-module))
                    (resolve-interface '(mod ...)))
       ...))))

;;; common.scm ends here.
