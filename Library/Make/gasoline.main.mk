### gasoline.main.mk -- Project Makefile

# Author: Michael Grünewald
# Date: Tue Apr 15 00:04:36 CEST 2014

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

.include "gasoline.init.mk"

SUBDIR+=	${PROJECTMODULE}
SUBDIR+=	toplevel
SUBDIR+=	testsuite
SUBDIR+=	example
SUBDIR+=	manual

test: all
	@cd testsuite && ${MAKE} test

.include "bps.init.mk"
.include "bps.project.mk"

### End of file `gasoline.main.mk'
