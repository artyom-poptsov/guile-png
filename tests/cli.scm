;;; cli.scm -- CLI tests.

;; Copyright (C) 2026 Artyom V. Poptsov <poptsov.artyom@gmail.com>
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

;; This module contains tests for command-line interface (CLI.)


;;; Code:

(use-modules (srfi srfi-64)
             (srfi srfi-26)
             (ice-9 iconv)
             (oop goops)
             (png core common)
             (png core chunk)
             (png cli command-describe))


(define %test-name "cli")

(test-begin %test-name)



(define %ihdr-data #vu8(0 0 13 180 0 0 9 176 8 2 0 0 0))

(test-assert "print-chunk: known chunk"
  (with-output-to-string
    (lambda ()
      (print-chunk (make <png-chunk>
                     #:data   %ihdr-data
                     #:type   'IHDR
                     #:length 13
                     #:crc    (vector->int32 #vu8(119 50 219 167)))))))

(test-equal "print-chunk: unknown chunk"
  "** UKNOWN_TYPE\n   :length: 13\n   :crc: 1999821735\n   :END:\n"
  (with-output-to-string
    (lambda ()
      (print-chunk (make <png-chunk>
                     #:data   %ihdr-data
                     #:type   'UKNOWN_TYPE
                     #:length 13
                     #:crc    (vector->int32 #vu8(119 50 219 167)))))))


(define exit-status (test-runner-fail-count (test-runner-current)))

(test-end %test-name)

(exit exit-status)

;; cli.scm ends here.
