### Makefile -- Unicode

# Author: Michael Grünewald
# Date: Mon Jun  2 11:44:40 CEST 2014

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

PROGRAM=	unicode

SRCS+=		main.ml

TESTS=		test-output-channel

test-output-channel.got: ${PROGRAM}
	./${PROGRAM} > ${.TARGET}

.include "gasoline.program.mk"
.include "gasoline.trip-trap.mk"

### End of file `Makefile'
