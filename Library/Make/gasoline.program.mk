### gasoline.program.mk -- Build a program

# Author: Michaël Grünewald
# Date: Thu Nov  7 07:39:41 CET 2013

# Gasoline (https://github.com/michipili/gasoline)
# This file is part of Gasoline
#
# Copyright © 2013 Michael Grünewald
#
# This file must be used under the terms of the CeCILL-B.
# This source file is licensed as described in the file COPYING, which
# you should have received as part of this distribution. The terms
# are also available at
# http://www.cecill.info/licences/Licence_CeCILL-B_V1-en.txt

PROGRAM?= ${.CURDIR:T}
ODOC_NAME?= ${PROGRAM}

.include "gasoline.init.mk"

.for module in ${PROJECTMODULE}
LIBS+= gasoline_${module}
.endfor

.include "ocaml.prog.mk"

### End of file `gasoline.program.mk'
