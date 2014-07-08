(* CConfiguration -- Configuration for C-Stylish applications

Author: Michael Grünewald
Date: Sun May 12 10:46:09 CEST 2013

Copyright © 2013 Michael Grünewald

This file must be used under the terms of the CeCILL-B.
This source file is licensed as described in the file COPYING, which
you should have received as part of this distribution. The terms
are also available at
http://www.cecill.info/licences/Licence_CeCILL-B_V1-en.txt *)
open CType

include Configuration

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
