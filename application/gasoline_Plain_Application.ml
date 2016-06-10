(* Gasoline_Plain_Application -- C-Stylish Applications

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
open Printf

module Internal =
  Gasoline_Generic_Application.Make(Gasoline_Plain_Definition.Value)

module Component =
  Internal.Component

module Configuration =
struct
  type component =
    Internal.Component.t

  let make value_of_string component
      ?optarg ?flag ?env ?shy name default description =
    Internal.Configuration.make value_of_string component
      ?optarg ?flag ?env ?shy name default description

  let _make value_of_string name =
    make
      (fun s ->
         try value_of_string s
         with Invalid_argument(_) | Failure(_) -> failwith name)

  let make_bool =
    _make bool_of_string "boolean"

  let make_int =
    _make int_of_string "integer"

  let make_float =
    _make float_of_string "floating point"

  let make_string =
    _make (fun s -> s) "string"

  type spec = Internal.Configuration.spec =
    | Empty
    | Command_line
    | Environment
    | OptionalFile of string
    | ImportantFile of string
    | UserFile of (unit -> string)
    | Heredoc of string
    | Alist of ((string list * string) * string) list
    | Merge of spec * spec
    | Override of spec * spec
end

let run =
  Internal.run
