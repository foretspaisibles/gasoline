### gasoline.trip-trap.mk -- Trip-Trap testing

# Author: Michael Grünewald
# Date: Wed Apr 16 11:55:06 CEST 2014

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

# Variables:
#
# TESTENV
#  Environment variables to be passed to the test programs
#
#   The value of the TESTENV variable should be a valid argument for
#   `env(1)', for instance
#
#     TESTENV=	LANG=en_US.UTF-8 LC_COLLATE=C

.include "bps.init.mk"
.include "gasoline.init.mk"


#
# Prepare the Gasoline-OCaml compiler
#

GASOLINEOCAMLC=	ocamlfind ocamlc -linkpkg -package "${PKGS}"
.for dir in ${DIRS}
GASOLINEOCAMLC+= -I ${dir}
.endfor
.for module in ${PROJECTMODULE}
GASOLINELIBS+= gasoline_${module}.cma
.endfor
.if defined(WITH_DEBUG)&&(${WITH_DEBUG} == "yes")
GASOLINEOCAMLC+= -g
.endif

.SUFFIXES: .cma
.PATH.cma: ${DIRS}


#
# Rules for tests
#

all:
	${NOP}

test:
	${NOP}

.for test in ${TESTS}
CLEANFILES+= ${test}.got

test: do-${test}

.if !target(do-${test})
do-${test}: ${test}.expected ${test}.got
	diff -u ${test}.expected ${test}.got
.endif

.if !target(${test}.got) && exists(${test}.sh)
${test}.got:
	@sh ${test}.sh > ${test}.got
.endif

.if !target(${test}.got) && exists(${test}.ml)
${test}.got: ${test}.byte
.if defined(TESTENV)
	${ENVTOOL} ${TESTENV} ./${test}.byte > ${test}.got
.else
	./${test}.byte > ${test}.got
.endif
${test}.byte: ${GASOLINELIBS} ${test}.ml
	${GASOLINEOCAMLC} ${.ALLSRC:M*.cma} -o ${test}.byte ${test}.ml
CLEANFILES+= ${test}.byte
CLEANFILES+= ${test}.cmo
CLEANFILES+= ${test}.cmi
.endif

.endfor

do-clean: do-clean-log

do-clean-log:
	${RM} -f *.log


.include "bps.clean.mk"

### End of file `gasoline.trip-trap.mk'
