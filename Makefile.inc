### Makefile.inc -- Project configuration

# Author: Michael Grünewald
# Date: Sat Dec 27 11:37:01 CET 2014

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

.if "${THISMODULE}" == "ocaml.lib"
OCAMLCFLAGS+=	-warn-error +3

ODOC_NAME=	${LIBRARY}
ODOC_FORMAT=	odoc
ODOC_TITLE=	Ocamldoc dump for ${LIBRARY}
.endif


.if "${THISMODULE}" == "ocaml.toplevel"
EXTERNAL+=	ocaml.findlib:compiler-libs
.endif

# Local Variables:
# mode: makefile
# End:

### End of file `Makefile.inc'
