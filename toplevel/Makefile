### Makefile -- Gasoline toplevel

# Author: Michael Grünewald
# Date: Tue Jun  3 12:43:36 CEST 2014

# Gasoline (https://github.com/michipili/gasoline)
# This file is part of Gasoline
#
# Copyright © 2014 Michael Grünewald
#
# This file must be used under the terms of the CeCILL-B.
# This source file is licensed as described in the file COPYING, which
# you should have received as part of this distribution. The terms
# are also available at
# http://www.cecill.info/licences/Licence_CeCILL-B_V1-en.txt

TOPLEVEL=		gasoline

SRCS+=			gasoline_config.ml
SRCS+=			gasoline_toplevel.ml


BPS_AUTOCONF_VARIABLE=	LIBDIR
BPS_AUTOCONF_VARIABLE+=	exec_prefix
BPS_AUTOCONF_VARIABLE+=	prefix
BPS_AUTOCONF_VARIABLE+=	bindir
BPS_AUTOCONF_VARIABLE+=	libdir

_BPS_AUTOCONF_REPLACE=
_BPS_AUTOCONF_EXPAND=

.for v in ${BPS_AUTOCONF_VARIABLE}
_BPS_AUTOCONF_REPLACE+=	-e 's|[@]${v}@|$$(${v})|g'
_BPS_AUTOCONF_EXPAND+=	-e 's|[$$]{${v}}|${${v}}|g'
_BPS_AUTOCONF_EXPAND+=	-e 's|[$$](${v})|${${v}}|g'
.endfor

BPS_AUTOCONF_REPLACE=	${_BPS_AUTOCONF_REPLACE} ${_BPS_AUTOCONF_EXPAND}

gasoline_config.ml: gasoline_config.ml.in
	${SED} ${BPS_AUTOCONF_REPLACE} ${.ALLSRC} > ${.TARGET}

CLEANFILES+=		gasoline_config.ml

.include "ocaml.toplevel.mk"
