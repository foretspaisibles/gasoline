(* Test_configfile_syntax -- Demonstrate syntax error in the config file

Author: Michael Grünewald
Date: Sun Sep 21 01:21:22 CEST 2014

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

let () =
  let configuration =
    let open Application.Configuration in
    ImportantFile Sys.argv.(1) in
  Application.run
    "test_config_file_syntax" "" ""
    ~configuration
    ignore
