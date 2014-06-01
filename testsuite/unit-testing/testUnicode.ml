(* TestUnicode -- Testing our unicode framework

Author: Michael Grünewald
Date: Sun Jun  1 11:15:28 CEST 2014

Gasoline (https://github.com/michipili/gasoline)
This file is part of Gasoline

Copyright © 2014 Michael Grünewald

This file must be used under the terms of the CeCILL-B.
This source file is licensed as described in the file COPYING, which
you should have received as part of this distribution. The terms
are also available at
http://www.cecill.info/licences/Licence_CeCILL-B_V1-en.txt *)

open UnitTest


(* Test the length of a string.

We compare the length of several U8-encoded string with their expected
length. *)
module Length =
struct

  let challenge = [
    "éléphant", 8;
    "François", 8;
    "sœur", 4;
  ]

  let length text =
    let open Unicode in
    UString.length (u8 text)

  let assert_length (text, expected) =
    assert_int text length text expected

  let init suite =
    List.iter (add_case suite) (List.map assert_length challenge)

end

let init suite =
  Length.init suite

let () = with_registered_suite "Unicode" init
