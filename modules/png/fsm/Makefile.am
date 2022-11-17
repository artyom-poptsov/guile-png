## Makefile.am -- Config file for GNU Automake.
##
## Copyright (C) 2022 Artyom V. Poptsov <poptsov.artyom@gmail.com>
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
## along with Guile-ICS.  If not, see <http://www.gnu.org/licenses/>.

include $(top_srcdir)/build-aux/am/guile.am

SOURCES = \
	chunk-context.scm	\
	png-context.scm		\
	signature-context.scm

BUILT_SOURCES =	\
	context.scm \
	png-parser.scm \
	chunk-parser.scm \
	signature-parser.scm

INSTALL += \
	context.scm \
	png-parser.scm \
	chunk-parser.scm \
	signature-parser.scm

EXTRA_DIST += \
	chunk-parser.puml \
	png-parser.puml \
	signature-parser.puml

moddir=$(datadir)/guile/site/$(GUILE_EFFECTIVE_VERSION)/png/fsm
godir=$(libdir)/guile/$(GUILE_EFFECTIVE_VERSION)/site-ccache/png/fsm

SUFFIXES = .puml

GUILE_SMC_CONTEXT_ARGS = \
	--log-driver file \
	--log-opt "file=context.log" \
	-m "(png fsm context)"

GUILE_SMC_SIGNATURE_PARSER_ARGS = \
	--log-driver file \
	--log-opt "file=signature-parser.log" \
	-L $(abs_top_srcdir)/modules:$(abs_top_builddir)/modules \
	--fsm-name signature-parser \
	--fsm-module "(png fsm signature-parser)" \
	-U "((png fsm context) (png fsm signature-context))" \
	--validate

GUILE_SMC_CHUNK_PARSER_ARGS = \
	--log-driver file \
	--log-opt "file=chunk-parser.log" \
	-L $(abs_top_srcdir)/modules:$(abs_top_builddir)/modules \
	--fsm-name chunk-parser \
	--fsm-module "(png fsm chunk-parser)" \
	-U "((png fsm context) (png fsm chunk-context))" \
	--validate

GUILE_SMC_PNG_PARSER_ARGS = \
	--log-driver file \
	--log-opt "file=png-parser.log" \
	-L $(abs_top_srcdir)/modules:$(abs_top_builddir)/modules \
	--fsm-name png-parser \
	--fsm-module "(png fsm png-parser)" \
	-U "((png fsm context) (png fsm png-context))" \
	--validate

if ENABLE_SMC_STANDALONE_MODE
GUILE_SMC_CONTEXT_ARGS += \
	--standalone
GUILE_SMC_SIGNATURE_PARSER_ARGS += \
	--target=guile-standalone
GUILE_SMC_CHUNK_PARSER_ARGS += \
	--target=guile-standalone
GUILE_SMC_PNG_PARSER_ARGS += \
	--target=guile-standalone
endif

GUILE_SMC_ENV = GUILE_AUTO_COMPILE=0

context.scm:
	@echo "  SMC      $@"
	@$(GUILE_SMC_ENV) smc context $(GUILE_SMC_CONTEXT_ARGS) > $@

signature-parser.scm: context.go signature-context.go signature-parser.puml
	@echo "  SMC      $@"
	@$(GUILE_SMC_ENV) smc compile $(GUILE_SMC_SIGNATURE_PARSER_ARGS) signature-parser.puml > $@

chunk-parser.scm: context.go signature-parser.scm chunk-context.go chunk-parser.puml
	@echo "  SMC      $@"
	@$(GUILE_SMC_ENV) smc compile $(GUILE_SMC_CHUNK_PARSER_ARGS) chunk-parser.puml > $@

png-parser.scm: context.go chunk-parser.go png-context.go png-parser.puml
	@echo "  SMC      $@"
	@$(GUILE_SMC_ENV) smc compile $(GUILE_SMC_PNG_PARSER_ARGS) png-parser.puml > $@

png-context.scm: signature-parser.scm chunk-parser.scm

# XXX: Automake does not remove those files for some reason.
uninstall-hook:
	rm -f $(DESTDIR)$(datadir)/guile/site/$(GUILE_EFFECTIVE_VERSION)/png/fsm/context.scm \
		$(DESTDIR)$(libdir)/guile/$(GUILE_EFFECTIVE_VERSION)/site-ccache/png/fsm/context.scm

CLEANFILES += \
	context.log	\
	context.scm	\
	context.go	\
	png-parser.scm \
	png-parser.log	\
	png-parser.go	\
	signature-parser.scm	\
	signature-parser.log	\
	signature-parser.go	\
	chunk-parser.scm	\
	chunk-parser.log	\
	chunk-parser.go

### Makefile.am ends here.
