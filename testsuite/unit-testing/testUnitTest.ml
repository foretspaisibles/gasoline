(* TestUnitTest -- Testing our unit testing framework

Author: Michael Grünewald
Date: Mon Apr 14 23:55:31 CEST 2014

Gasoline (https://github.com/michipili/gasoline)
This file is part of Gasoline

Copyright © 2014 Michael Grünewald

This file must be used under the terms of the CeCILL-B.
This source file is licensed as described in the file COPYING, which
you should have received as part of this distribution. The terms
are also available at
http://www.cecill.info/licences/Licence_CeCILL-B_V1-en.txt *)

open UnitTest

let init suite =
  ignore suite

let () = with_registered_suite "UnitTest" init
