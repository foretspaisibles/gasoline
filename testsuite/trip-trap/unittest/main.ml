(* Main -- Unit testing

Author: Michael Grünewald
Date: Wed Apr 16 11:26:30 CEST 2014

Gasoline (https://github.com/michipili/gasoline)
This file is part of Gasoline

Copyright © 2014 Michael Grünewald

This file must be used under the terms of the CeCILL-B.
This source file is licensed as described in the file COPYING, which
you should have received as part of this distribution. The terms
are also available at
http://www.cecill.info/licences/Licence_CeCILL-B_V1-en.txt *)

open UnitTest


module DatabaseOracle =
struct
  let init suite =
    List.iter (add_case suite) [
      assert_success "connectivity";
      assert_success "drop-table";
    ]
  let () = with_registered_suite "oracle" init
end


module DatabaseSQLite =
struct
  let init suite =
    List.iter (add_case suite) [
      assert_success "insert 1000 entries";
    ]
  let () = with_registered_suite "sqlite" init
end


module Database =
struct
  let init suite =
    ignore suite
(*    List.iter (add_suite suite) [
      "oracle";
      "sqlite";
    ] *)
  let () = with_registered_suite "database" init
end


let () = UnitTest.main ()
