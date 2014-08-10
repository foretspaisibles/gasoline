### Makefile -- Test the Getopt module

# Author: Michael Grünewald
# Date: Sun Aug 10 14:47:34 CEST 2014

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

PROGRAM=	test-getopt

SRCS+=		main.ml

TESTS=		test-help-screen
TESTS+=		test-cluster
TESTS+=		test-terminator
TESTS+=		test-bool
TESTS+=		test-int
TESTS+=		test-char
TESTS+=		test-string
TESTS+=		test-float
TESTS+=		test-rest

ARGS.test-help-screen=	-h
ARGS.test-cluster=	-avava -avsva
ARGS.test-terminator=	-a -s -- -- -a
ARGS.test-bool=		-b true -b false
ARGS.test-int=		-i 42
ARGS.test-char=		-c X
ARGS.test-string=	-s text
ARGS.test-float=	-f 0.1
ARGS.test-rest=		rest1 rest2

.for test in ${TESTS}
${test}.got: ${PROGRAM}
	./${PROGRAM} ${ARGS.${test}} > ${.TARGET}
.endfor

.include "gasoline.program.mk"
.include "gasoline.trip-trap.mk"

### End of file `Makefile'
