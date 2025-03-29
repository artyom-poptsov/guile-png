## Makefile.am -- Config file for GNU Automake.
##
## Copyright (C) 2025 Artyom V. Poptsov <poptsov.artyom@gmail.com>
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
## along with Guile-PNG.  If not, see <http://www.gnu.org/licenses/>.

bin_SCRIPTS = \
	png

EXTRA_DIST = \
	png.in

SUFFIXES = .in

substitute = sed -e 's,[@]GUILE[@],$(GUILE),g'

.in:
	@echo "  GEN      $@"
	@$(substitute) "$^" > "$@.tmp" && 	\
	chmod +x "$@.tmp" &&			\
	mv "$@.tmp" "$@"

CLEANFILES = \
	png

### Makefile.am ends here.
