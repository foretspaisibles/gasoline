### Makefile -- Trip-trap test for the application kit

# Author: Michael Grünewald
# Date: Sun Sep 21 00:25:24 CEST 2014

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

TESTS+=		test_bootstrap_failure
TESTS+=		test_configfile_syntax
TESTS+=		test_convert_config
TESTS+=		test_convert_environment
TESTS+=		test_convert_getopt
TESTS+=		test_cyclic_dependency
TESTS+=		test_execution_failure
TESTS+=		test_execution_shutdown_failure
TESTS+=		test_help
TESTS+=		test_rcorder
TESTS+=		test_usage

test_convert_environment.got: punishment.byte
	env NUMBER='@' ./${.ALLSRC} > ${.TARGET}

test_convert_getopt.got: punishment.byte
	./${.ALLSRC} -n '@' > ${.TARGET}

test_convert_config.got: punishment.byte
	-./${.ALLSRC} -c punishment.conf > ${.TARGET} 2>&1

test_help.got: punishment.byte
	./${.ALLSRC} -h > ${.TARGET}

test_usage.got: punishment.byte
	-./${.ALLSRC} -x > ${.TARGET} 2>&1

.for test in ${TESTS}
.if!target(${test}.got)
${test}.got: ${test}.byte
.if defined(TESTENV)
	-${ENVTOOL} ${TESTENV} ./${test}.byte > ${test}.got 2>&1
.else
	-./${test}.byte > ${test}.got 2>&1
.endif
.endif
.if!target(${test}.byte)&&exists(${test}.ml)
${test}.byte: ${test}.ml
	${GASOLINEOCAMLC} ${.ALLSRC:M*.cma} -o ${test}.byte ${test}.ml
CLEANFILES+= ${test}.byte
CLEANFILES+= ${test}.cmo
CLEANFILES+= ${test}.cmi
.endif
.endfor

.include "gasoline.trip-trap.mk"

punishment.byte: ${GASOLINELIBS} punishment.ml
	${GASOLINEOCAMLC} ${.ALLSRC:M*.cma} -o ${.TARGET} ${.ALLSRC:M*.ml}

CLEANFILES+= punishment.byte
CLEANFILES+= punishment.cmo
CLEANFILES+= punishment.cmi

.for test in ${TESTS}
.if target(${test}.byte)
${test}.byte: ${GASOLINELIBS}
.endif
.endfor

### End of file `Makefile'
