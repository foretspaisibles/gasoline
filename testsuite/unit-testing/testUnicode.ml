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

open Broken


let assert_ustring id ?expected_failure f x y =
  let open Gasoline_Unicode in
  assert_equal id ?expected_failure
    ~printer:(UString.format Encoding.utf8)
    ~equal:UString.equal
    f x y


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
    let open Gasoline_Unicode in
    UString.length (u8 text)

  let assert_length (text, expected) =
    assert_int text length text expected

  let suite () =
    make_suite "length" "validate length computation"
    |@ List.map assert_length challenge
end


(* Test case change *)
module Changecase =
struct
  open Gasoline_Unicode

  let challenge = [
    "éléphant", "ÉLÉPHANT";
    "français", "FRANÇAIS";
    "sœur", "SŒUR";
  ]

  let assert_ll (lowercase,_) =
    assert_ustring ("ll-" ^ lowercase)
      UString.lowercase (u8 lowercase) (u8 lowercase)

  let assert_uu (_, uppercase) =
    assert_ustring ("uu-" ^ uppercase)
      UString.uppercase (u8 uppercase) (u8 uppercase)

  let assert_lu (lowercase, uppercase) =
    assert_ustring ("lu-" ^ lowercase)
      UString.uppercase (u8 lowercase) (u8 uppercase)

  let assert_ul (lowercase, uppercase) =
    assert_ustring ("ul-" ^ uppercase)
      UString.lowercase (u8 uppercase) (u8 lowercase)

  let suite () =
    make_suite "changecase" "validate case transformations"
    |@ Lemonade_List.Infix.
      ((@@) <$> [ assert_ll; assert_uu; assert_ul; assert_lu ] <*> challenge)
    |& assert_lu ("daß", "DASS")
end


(* Test encoding procedures *)
module Transcoding =
struct
  let suite () =
    make_suite "transcoding" "validate transcoding"
    |& assert_exception "udecode" Gasoline_Unicode.Encoding.Malformed_code
      Gasoline_Unicode.Encoding.(decode utf8 ) "\xC0"
    |& assert_exception "find" Not_found
      Gasoline_Unicode.Encoding.find "this is not the name of an encoding"
end

let () =
  make_suite "Unicode" "validate unicode operations"
  |* Length.suite ()
  |* Changecase.suite ()
  |* Transcoding.suite ()
  |> register
