### Makefile -- UnitTest

# Author: Michael Grünewald
# Date: Wed Apr 16 11:23:32 CEST 2014

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

PROGRAM=	unit-testing

SRCS+=		main.ml

PKGS+=		unix

TESTS=		test-help-screen
TESTS+=		test-primary-suites-list
TESTS+=		test-all-suites-list
TESTS+=		test-expected-failures-list

test-help-screen.got: ${PROGRAM}
	./${PROGRAM} -h 2> ${.TARGET}

test-primary-suites-list.got: ${PROGRAM}
	./${PROGRAM} -l > ${.TARGET}

test-all-suites-list.got: ${PROGRAM}
	./${PROGRAM} -a > ${.TARGET}

test-expected-failures.got: ${PROGRAM}
	./${PROGRAM} -x > ${.TARGET}

.include "gasoline.program.mk"
.include "gasoline.trip-trap.mk"

### End of file `Makefile'
