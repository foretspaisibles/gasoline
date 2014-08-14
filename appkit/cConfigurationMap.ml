(* CConfigurationMap -- Configuration map for C-Stylish applications

Author: Michael Grünewald
Date: Sun May 12 10:46:09 CEST 2013

Copyright © 2013 Michael Grünewald

This file must be used under the terms of the CeCILL-B.
This source file is licensed as described in the file COPYING, which
you should have received as part of this distribution. The terms
are also available at
http://www.cecill.info/licences/Licence_CeCILL-B_V1-en.txt *)
open CType
include ConfigurationMap

module Concrete =
struct
  let wrap f x =
    try f x
    with Failure _ -> ConfigurationMap.parse_error Lexing.dummy_pos x

  let bool_of_string s =
    wrap Pervasives.bool_of_string s

  let char_of_string s =
    if String.length s = 1 then
      s.[0]
    else ConfigurationMap.parse_error Lexing.dummy_pos s

  let int_of_string s =
    wrap Pervasives.int_of_string s

  let float_of_string s =
    wrap Pervasives.float_of_string s

  let string_of_string s =
    s

  let string_of_char c =
    String.make 1 c


  let bool = {
    of_string = bool_of_string;
    to_string = string_of_bool;
  }

  let char = {
    of_string = char_of_string;
    to_string = string_of_char;
  }

  let int = {
    of_string = int_of_string;
    to_string = string_of_int;
  }

  let float = {
    of_string = float_of_string;
    to_string = string_of_float;
  }

  let string = {
    of_string = string_of_string;
    to_string = string_of_string;
  }
end

let key_bool =
  key Concrete.bool

let key_char =
  key Concrete.char

let key_int =
  key Concrete.int

let key_float =
  key Concrete.float

let key_string =
  key Concrete.string
