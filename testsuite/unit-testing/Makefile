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

PROGRAM=		unit_testing
PROGRAM+=		rcorder_testing

SRCS.unit_testing=	testUnicode.ml
SRCS.unit_testing+=	testPlainDefinitionValue.ml
SRCS.unit_testing+=	main.ml

#SRCS.rcorder_testing=	rcorder_testing.ml

OCAMLLFLAGS+=		-linkall

.for program in ${PROGRAM}
do-test: do-test-${program}
do-test-${program}: ${program}
	${.ALLSRC:tA}
.endfor

do-install:
	${NOP}

.include "ocaml.prog.mk"

### End of file `Makefile'
