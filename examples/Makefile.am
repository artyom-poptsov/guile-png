## Copyright (C) 2023 Artyom V. Poptsov <poptsov.artyom@gmail.com>
##
## This file is part of Guile-PNG.
##
## Guile-PNG is free software: you can redistribute it and/or
## modify it under the terms of the GNU General Public License as
## published by the Free Software Foundation, either version 3 of the
## License, or (at your option) any later version.
##
## Guile-PNG is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
## General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Guile-PNG.  If not, see
## <http://www.gnu.org/licenses/>.

bin_SCRIPTS = \
	image-processing-invert-colors.scm \
	image-processing-solarize.scm \
	print-chunks.scm \
	sierpinski-triangle.scm \
	sierpinski-carpet.scm \
	sierpinski-carpet-animation.scm

sssh_scm_SOURCES = sssh.scm.in
ssshd_scm_SOURCES = ssshd.scm.in

SOURCES = \
	image-processing-invert-colors.scm.in \
	image-processing-solarize.scm.in \
	print-chunks.scm.in \
	sierpinski-triangle.scm.in \
	sierpinski-carpet.scm.in \
	sierpinski-carpet-animation.scm.in

EXTRA_DIST = $(SOURCES)

examplesdir = $(pkgdatadir)/examples
dist_examples_DATA = \
	$(bin_SCRIPTS)

substitute = sed -e 's,[@]GUILE[@],$(GUILE),g'

SUFFIXES = .in

.in:
	@echo "  GEN      $@"
	@$(MKDIR_P) "`dirname "$@"`"
	@$(substitute) "$^" > "$@.tmp" && 	\
	chmod +x "$@.tmp" &&			\
	mv "$@.tmp" "$@"

CLEANFILES = \
	$(bin_SCRIPTS)

## Makefile.am ends here.
