(* Test_bootstrap_failure -- Test bootstrap failure

Author: Michael Grünewald
Date: Sun Sep 21 00:26:23 CEST 2014

Gasoline (https://github.com/michipili/gasoline)
This file is part of Gasoline

Copyright © 2014 Michael Grünewald

This file must be used under the terms of the CeCILL-B.
This source file is licensed as described in the file COPYING, which
you should have received as part of this distribution. The terms
are also available at
http://www.cecill.info/licences/Licence_CeCILL-B_V1-en.txt *)
module Application =
  Gasoline_Plain_Application

module Monolith =
struct
  let comp = {
    Application.Component.
    name = "monolith";
    version = "1.0";
    require = [];
    provide = [];
    description = "The monolithic component of our application";
    config_prefix = [];
    getopt_prefix = None;
  }

  let bootstrap _ =
    failwith "This error was triggered on purpose."

  let () = Application.Component.register ~bootstrap comp
end

let () =
  Application.run
    "test_boostrap_failure" "" ""
    ignore
