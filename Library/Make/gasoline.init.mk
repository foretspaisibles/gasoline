### gasoline.init.mk -- Initialisation

# Author: Michael Grünewald
# Date: Thu Nov  7 07:35:59 CET 2013

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

PROJECTMODULE+=	appkit
PROJECTMODULE+=	testkit

PKGS+= camomile

.for module in ${PROJECTMODULE}
DIRS+= ${PROJECTBASE}/${module}
.endfor

PACKAGE=	gasoline
PACKAGEDIR=	/${PACKAGE}

### End of file `gasoline.init.mk'
