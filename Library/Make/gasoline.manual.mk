### gasoline.manual.mk -- Preparation a manual

# Author: Michaël Grünewald
# Date: Thu Nov  7 07:38:22 CET 2013

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

.include "gasoline.init.mk"

ODOC_TITLE=		Gasoline library
ODOC_HTML_CSS_FILE=	${PROJECTBASE}/Library/Data/gasoline.css
ODOC_HTML_INTRO=	intro.text
ODOC_HTML_CHARSET=	utf-8

.for module in ${PROJECTMODULE}
MANUAL+= gasoline_${module}.odoc
.endfor

test:
	${NOP}

.include "ocaml.manual.mk"

### End of file `gasoline.manual.mk'
