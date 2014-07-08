### Makefile -- Author kit

# Author: Michael Grünewald
# Date: Tue Jun  3 11:44:56 CEST 2014

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

PROGRAM=	author

SRCS+=		main.ml

TESTS=		test-simple-1-sgml
TESTS+=		test-simple-2-sgml
TESTS+=		test-simple-3-sgml
TESTS+=		test-simple-1-html

PROGRAM8=	${ENVTOOL} LANG=en_US.UTF-8 ./${PROGRAM}

.for test in ${TESTS}
${test}.got: ${PROGRAM}
	${PROGRAM8} ${.TARGET}
.endfor

.include "gasoline.program.mk"
.include "gasoline.trip-trap.mk"

### End of file `Makefile'
