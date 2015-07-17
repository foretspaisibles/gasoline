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
  type 'a kind = 'a Gasoline_Plain_Definition.Value.kind =
  | Bool : bool kind
  | Int : int kind
  | Char : char kind
  | String : string kind
  | Float : float kind

  type component =
    Internal.Component.t

  let make kind component ?flag ?env ?shy name default description =
    Internal.Configuration.make kind component ?flag ?env ?shy name default description

  type spec = Internal.Configuration.spec =
    | Empty
    | Command_line
    | Environment
    | File of string
    | UserFile of string list * string
    | Heredoc of string
    | Alist of ((string list * string) * string) list
    | Merge of spec * spec
    | Override of spec * spec
end

let run =
  Internal.run
