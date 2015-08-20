(* TestPlainDefinitionValue -- Testing Plain Definition Value

Author: Michael Grünewald
Date: Fri Aug  8 10:16:40 CEST 2014

Gasoline (https://github.com/michipili/gasoline)
This file is part of Gasoline

Copyright © 2014 Michael Grünewald

This file must be used under the terms of the CeCILL-B.
This source file is licensed as described in the file COPYING, which
you should have received as part of this distribution. The terms
are also available at
http://www.cecill.info/licences/Licence_CeCILL-B_V1-en.txt *)

open Broken
module Value = Gasoline_Plain_Definition.Value

type spec =
| Bool of bool
| Int of int
| Char of char
| String of string
| Float of float

let make_value spec =
  match spec with
  | Bool(v) -> Value.make Value.Bool v
  | Int(v) -> Value.make Value.Int v
  | Char(v) -> Value.make Value.Char v
  | String(v) -> Value.make Value.String v
  | Float(v) -> Value.make Value.Float v

let assert_convert (id, spec, s) =
  assert_equal id
    ~printer:Value.printer
    ~equal:Value.equal
    Value.of_string s (make_value spec)

let challenge = [
  "float-1", Float(0.1), "0.1";
  "float-2", Float(1E2), "1E2";
  "float-3", String("0.1x"), "0.1x";
]

let () =
  register_suite "PlainDefinition.Value" "validate dynamic types operations"
    (List.map assert_convert challenge)
