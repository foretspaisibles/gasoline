### Makefile -- Trip-trap test for the application kit

# Author: Michael Grünewald
# Date: Sun Sep 21 00:25:24 CEST 2014

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

TEST+=		test_bootstrap_failure
TEST+=		test_configfile_syntax
TEST+=		test_convert_config
TEST+=		test_convert_environment
TEST+=		test_convert_getopt
TEST+=		test_cyclic_dependency
TEST+=		test_execution_failure
TEST+=		test_execution_shutdown_failure
TEST+=		test_help
TEST+=		test_rcorder
TEST+=		test_usage

PROGRAM=	punishment

FIXPATH=	${SED_INPLACE} -e "s@'[^']*/@'@"

test_configfile_syntax.got: test_configfile_syntax.byte test_configfile_syntax.conf
	-${.ALLSRC:M*.byte:tA} ${.ALLSRC:M*.conf:tA} 2> ${.TARGET}
	${FIXPATH} ${.TARGET}

test_bootstrap_failure.got: test_bootstrap_failure.byte
	-./${.ALLSRC} 2> ${.TARGET}

test_convert_environment.got: punishment.byte
	-env NUMBER='@' ./${.ALLSRC} > ${.TARGET} 2>&1

test_convert_getopt.got: punishment.byte
	-./${.ALLSRC} -n '@' > ${.TARGET} 2>&1

test_convert_config.got: punishment.byte punishment.conf
	-${.ALLSRC:M*.byte:tA} -c ${.ALLSRC:M*.conf:tA} > ${.TARGET} 2>&1
	${FIXPATH} ${.TARGET}

test_cyclic_dependency.got: test_cyclic_dependency.byte
	./${.ALLSRC} 2> ${.TARGET} || [ $$? -eq 70 ]

test_execution_failure.got: test_execution_failure.byte
	./${.ALLSRC} 2> ${.TARGET} || [ $$? -eq 70 ]

test_execution_shutdown_failure.got: test_execution_shutdown_failure.byte
	./${.ALLSRC} 2> ${.TARGET} || [ $$? -eq 70 ]

test_help.got: punishment.byte
	./${.ALLSRC} -h > ${.TARGET}

test_usage.got: punishment.byte
	./${.ALLSRC} -x > ${.TARGET} 2>&1 || [ $$? -eq 64 ]

.PATH: ${SRCDIR}/example/punishment

.include "gasoline.trip-trap.mk"

### End of file `Makefile'
