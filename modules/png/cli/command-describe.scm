;;; command-describe.scm -- CLI for describing PNG content.

;; Copyright (C) 2025-2026 Artyom V. Poptsov <poptsov.artyom@gmail.com>
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

;; This module contains the command line interface for getting various
;; human-readable information about PNG file internals.


;;; Code:


(define-module (png cli command-describe)
  #:use-module (ice-9 getopt-long)
  #:use-module (ice-9 hash-table)
  #:use-module (ice-9 format)
  #:use-module (srfi srfi-1)
  #:use-module (png)
  #:use-module (png image)
  #:use-module (png core chunk)
  #:use-module (png core chunk ihdr)
  #:use-module (png core chunk bkgd)
  #:use-module (png core chunk chrm)
  #:use-module (png core chunk iccp)
  #:use-module (png core chunk phys)
  #:use-module (png core chunk time)
  #:use-module (png core chunk trns)
  #:use-module (png core chunk text)
  #:use-module (png core chunk ornt)
  #:export (command-describe
            print-chunk))

(define (print-help)
  (display "\
Usage: png describe [options] [input-file]

If no input file is provided, then the program reads the image data from the
current input port.

Options:
  --help, -h                 Print this message and exit.
"))

(define %option-spec
  '((help                     (single-char #\h) (value #f))))



(define (print-info lst)
  (for-each (lambda (e)
              (format #t "*** ~a: ~a~%" (car e) (cdr e)))
            lst))

(define (ihdr->list chunk)
  (list
   (cons 'width (png-chunk:IHDR-width chunk))
   (cons 'height (png-chunk:IHDR-height chunk))
   (cons 'bit-depth (png-chunk:IHDR-bit-depth chunk))
   (cons 'color-type (png-chunk:IHDR-color-type chunk))
   (cons 'compression-method (png-chunk:IHDR-compression-method chunk))
   (cons 'filter-method (png-chunk:IHDR-filter-method chunk))
   (cons 'interlace-method (png-chunk:IHDR-interlace-method chunk))))

(define (text->list chunk)
  (list
   (cons 'keyword (png-chunk:tEXT-keyword chunk))
   (cons 'text (png-chunk:tEXT-text chunk))))

(define (bkgd->list chunk)
  (list
   (cons 'color-type (png-chunk:bKGD-color-type chunk))
   (cons 'grayscale  (png-chunk:bKGD-grayscale chunk))
   (cons 'red        (png-chunk:bKGD-red chunk))
   (cons 'green      (png-chunk:bKGD-green chunk))
   (cons 'blue       (png-chunk:bKGD-blue chunk))
   (cons 'palette-index (png-chunk:bKGD-palette-index chunk))))

(define (chrm->list chunk)
  (list
   (cons 'white-point-x (png-chunk:cHRM-white-point-x chunk))
   (cons 'white-point-y (png-chunk:cHRM-white-point-y chunk))
   (cons 'red-x         (png-chunk:cHRM-red-x chunk))
   (cons 'red-y         (png-chunk:cHRM-red-y chunk))
   (cons 'green-x       (png-chunk:cHRM-green-x chunk))
   (cons 'green-y       (png-chunk:cHRM-green-y chunk))
   (cons 'blue-x        (png-chunk:cHRM-blue-x chunk))
   (cons 'blue-y        (png-chunk:cHRM-blue-y chunk))))

(define (iccp->list chunk)
  (list
   (cons 'profile-name       (png-chunk:iCCP-profile-name chunk))
   (cons 'compression-method (png-chunk:iCCP-compression-method chunk))))
;   (cons 'profile            (png-chunk:iCCP-profile chunk))))

(define (phys->list chunk)
  (list
   (cons 'pixels-per-unit-x-axis (png-chunk:pHYs-pixels-per-unit-x-axis chunk))
   (cons 'pixels-per-unit-y-axis (png-chunk:pHYs-pixels-per-unit-y-axis chunk))
   (cons 'unit-specifier         (png-chunk:pHYs-unit-specifier chunk))))

(define (time->list chunk)
  (list
   (cons 'year    (png-chunk:tIME-year chunk))
   (cons 'month   (png-chunk:tIME-month chunk))
   (cons 'day     (png-chunk:tIME-day chunk))
   (cons 'hour    (png-chunk:tIME-hour chunk))
   (cons 'minute  (png-chunk:tIME-minute chunk))
   (cons 'second  (png-chunk:tIME-second chunk))))

(define (trns->list chunk)
  (list
   (cons 'color-type (png-chunk:tRNS-color-type chunk))
   (cons 'value      (png-chunk:tRNS-value chunk))))

(define (ornt->list chunk)
  (list
   (cons 'orientation (png-chunk:orNT-orientation chunk))))



(define %converter-table
  (alist->hash-table
   `((IHDR . ,ihdr->list)
     (bKGD . ,bkgd->list)
     (cHRM . ,chrm->list)
     (iCCP . ,iccp->list)
     (iTXt . ,text->list)
     (pHYs . ,phys->list)
     (tIME . ,time->list)
     (tEXt . ,text->list)
     (tRNS . ,trns->list)
     (orNT . ,ornt->list))))



(define (print-chunk chunk)
  (let* ((type (png-chunk-type chunk))
         (converter (hash-ref %converter-table
                              type))
         (info (png-chunk-type-info type))
         (bytes (and info (list-ref info 1))))
    (if bytes
        (begin
          (format #t "** ~a: ~a~%" type (list-ref info 2))
          (format #t "   :PROPERTIES:~%")
          (format #t "   :bytes:~a~%"
                  (fold (lambda (b prev)
                          (string-append prev " " (number->string b)))
                        ""
                        (u8vector->list bytes)))
          (format #t "   :critical?: ~a~%"
                  (if (logbit? 5 (u8vector-ref bytes 0))
                      "false"
                      "true"))
          (format #t "   :private?: ~a~%"
                  (if (logbit? 5 (u8vector-ref bytes 1))
                      "true"
                      "false"))
          (format #t "   :safe-to-copy?: ~a~%"
                  (if (logbit? 5 (u8vector-ref bytes 3))
                      "true"
                      "false")))
        (begin
          (format #t "** ~a~%" type)))
    (unless (equal? type 'IDAT)
      (format #t "   :length: ~a~%" (png-chunk-length chunk))
      (format #t "   :crc: ~a~%" (png-chunk-crc chunk))
      (format #t "   :END:~%"))
    (when converter
      (print-info (converter chunk)))))

(define (print-idat idat-chunks)
  (print-chunk (car idat-chunks))
  (format #t "   :total-length: ~a~%"
          (fold (lambda (c prev)
                  (+ prev (png-chunk-length c)))
                0
                idat-chunks))
  (format #t "   :END:~%")
  (format #t "   | ~10a | ~10a | ~10a |~%"
          "index"
          "length"
          "crc")
  (format #t "   |-~10a-+-~10a-+-~10a-|~%"
          (make-string 10 #\-)
          (make-string 10 #\-)
          (make-string 10 #\-))
  (let loop ((chunks idat-chunks)
             (index  0))
    (unless (null? chunks)
      (let ((c (car chunks)))
        (format #t "   | ~10a | ~10a | ~10a |~%"
                index
                (png-chunk-length c)
                (png-chunk-crc c))
        (loop (cdr chunks)
              (+ index 1))))))


(define (command-describe args)
  (let* ((options          (getopt-long args %option-spec))
         (help-needed?     (option-ref options 'help  #f))
         (args             (option-ref options '()    #f)))

    (when help-needed?
      (print-help)
      (exit 0))

    (let* ((port   (if (null? args)
                       (current-input-port)
                       (let ((p (open-input-file (car args))))
                         (unless p
                           (error "Could not open a file" (car args)))
                         p)))
           (image (png->scm port))
           (chunks (png-image-chunks image))
           (chunks-without-idata-and-iend
            (filter (lambda (chunk)
                      (let ((type (png-chunk-type chunk)))
                        (and (not (equal? type 'IDAT))
                             (not (equal? type 'IEND)))))
                    chunks))
           (idat-chunks (filter (lambda (chunk)
                                  (equal? (png-chunk-type chunk) 'IDAT))
                                chunks))
           (iend-chunk (car (reverse chunks))))
      (format #t "* Image~a~%"
              (if (null? args)
                  ""
                  (format #f ": ~a" (car args))))
      (for-each print-chunk chunks-without-idata-and-iend)
      (print-idat idat-chunks)
      (print-chunk iend-chunk))))

;;; command-describe.scm ends here.
