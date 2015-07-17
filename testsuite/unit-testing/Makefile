### Makefile -- Test suite

# Author: Michael Grünewald
# Date: Mon Apr 14 23:52:37 CEST 2014

# GIT-Toolkit (https://github.com/michipili/gasoline)
# This file is part of Gasoline
#
# Copyright © 2014 Michael Grünewald
#
# This file must be used under the terms of the CeCILL-B.
# This source file is licensed as described in the file COPYING, which
# you should have received as part of this distribution. The terms
# are also available at
# http://www.cecill.info/licences/Licence_CeCILL-B_V1-en.txt

PROGRAM=	unit_testing

SRCS=		testUnicode.ml
SRCS+=		testPlainDefinitionValue.ml

SRCS+=		main.ml

OCAMLLFLAGS+=	-linkall

do-test: ${PROGRAM}
	${.ALLSRC:tA}

do-install:
	${NOP}

.include "ocaml.prog.mk"

### End of file `Makefile'
