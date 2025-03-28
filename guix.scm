;; guix.scm --- GNU Guix package recipe    -*- coding: utf-8 -*-
;;
;; Copyright (C) 2022-2025 Artyom V. Poptsov <poptsov.artyom@gmail.com>
;;
;; Author: Artyom V. Poptsov <poptsov.artyom@gmail.com>
;; Created: 17 October 2022
;;
;; This file is part of Guile-PNG.
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
;;
;; GNU Guix development package. To use as the basis for a development
;; environment, run:
;;
;;  guix shell -D -f guix.scm
;;
;; In the new shell, run:
;;
;;  autoreconf -vif && ./configure && make check
;;
;;; Code:


(use-modules (guix gexp)
             (guix packages)
             (guix licenses)
             (guix git-download)
             (guix build-system gnu)
             (gnu packages autotools)
             (gnu packages guile)
             (gnu packages guile-xyz)
             (gnu packages bash)
             (gnu packages man)
             (gnu packages pkg-config)
             (gnu packages texlive)
             (gnu packages texinfo))


(define %source-dir (dirname (current-filename)))


(define guile-png
  (package
    (name "guile-png")
    (version "git")
    (source (local-file %source-dir
                        #:recursive? #t
                        #:select? (git-predicate %source-dir)))
    (build-system gnu-build-system)
    (native-inputs
     (list autoconf
           automake
           pkg-config
           texinfo
           texlive
           help2man
           ;; needed when cross-compiling.
           guile-3.0
           guile-lib
           guile-zlib
           guile-smc))
    (inputs
     (list bash-minimal
           guile-3.0
           guile-lib
           guile-zlib))
    (propagated-inputs
     (list guile-smc))
    (arguments
     (list #:make-flags #~(list "GUILE_AUTO_COMPILE=0") ;to prevent guild warnings
           #:modules `(((guix build guile-build-system)
                        #:select (target-guile-effective-version))
                       ,@%default-gnu-modules)
           #:imported-modules `((guix build guile-build-system)
                                ,@%default-gnu-imported-modules)
           #:phases #~(modify-phases %standard-phases
                        (add-after 'install 'wrap-guilescript
                          (lambda _
                            (let* ((bin (string-append #$output "/bin"))
                                   (version (target-guile-effective-version))
                                   (scm (string-append "/share/guile/site/"
                                                       version))
                                   (go (string-append "/lib/guile/"
                                                      version
                                                      "/site-ccache")))
                              (wrap-program (string-append bin "/png")
                                `("GUILE_LOAD_PATH" prefix
                                  (,(string-append #$output scm)))
                                `("GUILE_LOAD_COMPILED_PATH" prefix
                                  (,(string-append #$output go))))))))))
    (home-page "https://github.com/artyom-poptsov/guile-png")
    (synopsis "Guile library for PNG format support")
    (description
     "@code{guile-png} is a GNU Guile library for working with the
@url{https://en.wikipedia.org/wiki/PNG, PNG format}.  This library provides
API for reading PNG data.")
    (license gpl3)))

guile-png

;;; guix.scm ends here.
