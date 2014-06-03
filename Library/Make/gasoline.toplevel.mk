### Makefile -- Gasoline

# Author: Michael Grünewald
# Date: Tue Jun  3 12:44:50 CEST 2014

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
TOPLEVEL?= ${.CURDIR:T}

.include "gasoline.init.mk"

.for s in ${PROJECTMODULE}
LIBS+= gasoline_${s}
.endfor

.include "ocaml.toplevel.mk"

### End of file `gasoline.toplevel.mk'
