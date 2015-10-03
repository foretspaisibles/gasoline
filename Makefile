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

PACKAGE=		gasoline
VERSION=		0.3.0-current
OFFICER=		michipili@gmail.com

MODULE=			ocaml.lib:application
MODULE+=		ocaml.lib:message
MODULE+=		ocaml.lib:author
MODULE+=		ocaml.toplevel:toplevel
MODULE+=		ocaml.meta:meta
MODULE+=		ocaml.manual:manual
MODULE+=		ocaml.prog:example/caesar
MODULE+=		ocaml.prog:example/daemon
MODULE+=		ocaml.prog:example/punishment
MODULE+=		ocaml.prog:example/wordcount
MODULE+=		ocaml.prog:example/wordgen

SUBDIR=			testsuite

EXTERNAL=		ocaml.findlib:broken
EXTERNAL+=		ocaml.findlib:camomile
EXTERNAL+=		ocaml.findlib:configuration
EXTERNAL+=		ocaml.findlib:getopts
EXTERNAL+=		ocaml.findlib:lemonade

CONFIGURE=		meta/gasoline.in
CONFIGURE+=		Makefile.config.in

.include "generic.project.mk"

### End of file `Makefile'
