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

PROGRAM+=		${TEST}

do-install:
	${NOP}

USES+=			test:expected

.include "ocaml.prog.mk"

### End of file `gasoline.trip-trap.mk'
