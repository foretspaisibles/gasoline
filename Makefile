### Makefile -- Gasoline

# Author: Michael Grünewald
# Date: Tue Nov  5 22:37:27 CET 2013

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

PACKAGE=	gasoline
AUTHOR=		Michael Grünewald

CONFIGURE=	meta/gasoline.in

PROJECTDISTEXCLUDE=	Wiki

.MAKEFLAGS: -I${.CURDIR}/Library/Make
.include "gasoline.main.mk"

### End of file `Makefile'
