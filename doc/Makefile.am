## Makefile.am -- Config file for GNU Automake.
##
## Copyright (C) 2017-2025 Artyom V. Poptsov <poptsov.artyom@gmail.com>
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

info_TEXINFOS = guile-png.texi

guile_png_TEXINFOS = \
	guile-png-installation.texi \
	guile-png.texi	\
	fdl.texi	\
	indices.texi

dist_man1_MANS =			\
	png.1

## Silent rules.
AM_V_HELP2MAN = $(AM_V_HELP2MAN_$(V))
AM_V_HELP2MAN_ = $(AM_V_HELP2MAN_$(AM_DEFAULT_VERBOSITY))
AM_V_HELP2MAN_0 = @echo "  HELP2MAN" $@;
##

gen_man =							\
	LANGUAGE= $(top_builddir)/pre-inst-env $(HELP2MAN)	\
	$(HELP2MANFLAGS)

HELP2MANFLAGS = --source=GNU --info-page=$(PACKAGE_TARNAME)

png.1: $(abs_top_builddir)/utils/png
	-$(AM_V_HELP2MAN)$(gen_man) --output="$@" "$<"

CLEANFILES = \
	png.1

## Makefile.am ends here
