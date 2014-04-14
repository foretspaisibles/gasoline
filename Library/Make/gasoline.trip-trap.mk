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
${test}.got:
	ocaml ${test}.ml > ${test}.got
.endif

.endfor

### End of file `gasoline.trip-trap.mk'
