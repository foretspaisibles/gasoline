(* TestConfiguration -- Test configuration

Author: Michael Grünewald
Date: Fri Aug  8 14:55:52 CEST 2014

Gasoline (https://github.com/michipili/gasoline)
This file is part of Gasoline

Copyright © 2014 Michael Grünewald

This file must be used under the terms of the CeCILL-B.
This source file is licensed as described in the file COPYING, which
you should have received as part of this distribution. The terms
are also available at
http://www.cecill.info/licences/Licence_CeCILL-B_V1-en.txt *)
open UnitTest

let configuration1 = "
maxusers = 25
message = \"I still cannot do much to help you, \\
           but I can demonstrate how configuration values \\
           can span over multiple lines!\"
"

let configuration2 = "maxusers = 10"

let concrete_string =
  let id_string = (fun (x : string) -> x) in {
    ConfigurationMap.
    of_string = id_string;
    to_string = id_string;
  }

let concrete_int = {
  ConfigurationMap.
  of_string = int_of_string;
  to_string = string_of_int;
}

let test_multiple_lines suite =
  let conf = ConfigurationMap.from_string configuration1 in
  let key = ConfigurationMap.key concrete_string [] "message"
    "This is the default value of a message"
    "A message displayed to the user in various circonstances."
  in
  let expected = "\
    I still cannot do much to help you, \
    but I can demonstrate how configuration values \
    can span over multiple lines!"
  in
  let case = assert_string "multiple-lines"
    (ConfigurationMap.get conf) key expected
  in
  add_case suite case

let test_override suite =
  let conf =
    ConfigurationMap.(override (from_string configuration1)
		     (from_string configuration2))
  in
  let key = ConfigurationMap.key concrete_int [] "maxusers"
    13
    "The maximal number of users"
  in
  let expected = 10 in
  let case = assert_int "override"
    (ConfigurationMap.get conf) key expected
  in
  add_case suite case

let init suite =
  List.iter (fun f -> f suite) [
    test_multiple_lines;
    test_override;
  ]

let () = with_registered_suite "Configuration" init
