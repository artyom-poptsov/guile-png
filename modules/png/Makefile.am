## Makefile.am -- Config file for GNU Automake.
##
## Copyright (C) 2021-2025 Artyom V. Poptsov <poptsov.artyom@gmail.com>
##
## This file is part of Guile-PNG.
##
## Guile-INI is free software: you can redistribute it and/or
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

include $(top_srcdir)/build-aux/am/guilec

SUBDIRS = \
	core	\
	fsm	\
	graphics	\
	image-processing \
	cli

SOURCES = \
	version.scm		\
	chunk-decoder.scm	\
	image.scm		\
	image-processing.scm	\
	graphics.scm

moddir=$(datadir)/guile/site/$(GUILE_EFFECTIVE_VERSION)/png
nobase_dist_mod_DATA = \
	$(SOURCES) \
	$(EXTRA_DIST)

ccachedir=$(libdir)/guile/$(GUILE_EFFECTIVE_VERSION)/site-ccache/png
nobase_nodist_ccache_DATA = $(GOBJECTS)

# Make sure source files are installed first, so that the mtime of
# installed compiled files is greater than that of installed source
# files.  See
# <http://lists.gnu.org/archive/html/guile-devel/2010-07/msg00125.html>
# for details.
guile_smc_install_go_files = install-nobase_dist_ccacheDATA
$(guile_smc_install_go_files): install-nobase_dist_modDATA

GUILE_WARNINGS = -Wunbound-variable -Warity-mismatch -Wformat
SUFFIXES = .scm .go .puml

guilec_opts = --load-path=$(abs_top_srcdir)/modules/
.scm.go:
	$(AM_V_GUILEC)$(guilec_env) $(GUILEC) $(guilec_opts) \
	--output=$@ $<

clean-go:
	-$(RM) $(GOBJECTS)

.PHONY: clean-go

version_substitute = \
	sed -e 's,[@]VERSION[@],$(PACKAGE_VERSION),g'
version.scm: version.scm.in
	@echo "  GEN      $@"
	@$(version_substitute) "$^" > "$@"

CLEANFILES = \
	$(GOBJECTS) \
	version.scm

### Makefile.am ends here.
