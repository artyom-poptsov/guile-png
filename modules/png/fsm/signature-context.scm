;;; signature-context.scm -- PNG signature parser context.

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

;; This module contains procedures for PNG signature parser FSM.


;;; Code:

(define-module (png fsm signature-context)
  #:use-module (oop goops)
  #:use-module (ice-9 binary-ports)
  #:use-module (png fsm context)
  #:export (<signature-context>
            guard:correct-first-byte?
            guard:letter-ctrl-z?

            event-source

            action:wrong-first-byte-error
            action:unexpected-eof-error
            action:unexpected-byte-error))

(define-class <signature-context> (<u8-context>))

(define event-source u8-context-event-source)


(define-public (guard:correct-first-byte? ctx byte)
  (equal? byte 137))

(define-public (guard:letter-ctrl-z? ctx byte)
  (u8:sub? ctx byte))


(define (action:wrong-first-byte-error ctx byte)
  (error "Wrong first byte" ctx byte))

(define (action:unexpected-eof-error ctx byte)
  (error "Unexpected end of file" ctx byte))

(define (action:unexpected-byte-error ctx byte)
  (error "Unexpected byte read" ctx byte))

;;; signature-context.scm ends here.
