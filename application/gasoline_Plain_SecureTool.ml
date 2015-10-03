(* Gasoline_Plain_SecureTool -- C-Stylish SecureTools

Author: Michael Grünewald
Date: Sat Dec 28 13:05:16 CET 2013

Gasoline (https://github.com/michipili/gasoline)
This file is part of Gasoline

Copyright © 2013 Michael Grünewald

This file must be used under the terms of the CeCILL-B.
This source file is licensed as described in the file COPYING, which
you should have received as part of this distribution. The terms
are also available at
http://www.cecill.info/licences/Licence_CeCILL-B_V1-en.txt *)
module Component =
  Gasoline_Plain_Application.Component

module Configuration =
  Gasoline_Plain_Application.Configuration

let run name usage description ?notes ?system_configuration ?user_configuration main =
  let maybe_important_file = function
    | Some(name) -> Some(Configuration.ImportantFile(name))
    | None -> None
  in
  let maybe_optional_file = function
    | Some(name) -> Some(Configuration.OptionalFile(name))
    | None -> None
  in
  Gasoline_Plain_Application.run
    ~configuration:Configuration.(
        List.fold_left (fun acc -> function
            | Some(conf) -> Merge(acc, conf)
            | None -> acc) Empty [
          maybe_important_file system_configuration;
          Some(Command_line);
          Some(Environment);
          maybe_optional_file user_configuration;
        ])
    name usage description ?notes main
