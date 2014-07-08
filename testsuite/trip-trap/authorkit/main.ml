(* Main -- Author kit

Author: Michael Grünewald
Date: Tue Jun  3 11:45:45 CEST 2014

Gasoline (https://github.com/michipili/gasoline)
This file is part of Gasoline

Copyright © 2014 Michael Grünewald

This file must be used under the terms of the CeCILL-B.
This source file is licensed as described in the file COPYING, which
you should have received as part of this distribution. The terms
are also available at
http://www.cecill.info/licences/Licence_CeCILL-B_V1-en.txt *)
open Unicode

module Test_WHTML =
struct
  open WHTML

  let document () =
    html [
      body [
	h1 (u"My first heading");
	p [c"My first paragraph."];
      ]
    ]

  let main ppt =
    WSGML.format ppt (document());
    Format.pp_print_flush ppt ()
end

module Test_WSGML =
struct
  open WSGML

  let document contents =
    let body = element ~block:true "body" contents in
    let html = element ~block:true "html" [ body] in
    make
      ~declaration:"<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
      ~dtd:"<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01//EN\">"
      [ html ]

  let h1 =
    element ~block:false "h1" [ pcdata(u"My First Heading") ]

  let h1attr =
    element
      ~block:false
      ~attr:[
	"id", u"this-header-as-such-a-long-identifier-it-has-indeeed";
	"class", u"this-class-as-quite-along-identifier-as-well";
      ]
      "h1" [ pcdata(u"My First Heading") ]

  let p =
    element ~block:false "p" [ pcdata(u"My first paragraph.") ]

  let plong =
    element ~block:false "p" [ pcdata(u"This rather long paragraph \
    is used to demonstrate how text is folded within a regular paragraph.") ]

  let testsimple contents ppt =
    WSGML.format ppt (document contents);
    Format.pp_print_flush ppt ()

  let test1 = testsimple [ h1; p ]
  let test2 = testsimple [ h1; plong ]
  let test3 = testsimple [ h1attr; p ]

end

let testtable = [
  "test-simple-1-sgml",		Test_WSGML.test1;
  "test-simple-2-sgml",		Test_WSGML.test2;
  "test-simple-3-sgml",		Test_WSGML.test3;
]

let runtest filename =
  let open Filename in
  let suffix = ".got" in
  let testname =
    if check_suffix filename suffix then
      chop_suffix filename suffix
    else
      filename
  in
  let testfun =
    try List.assoc testname testtable
    with Not_found -> failwith("Unknown test: " ^ testname)
  in
  let chan = open_out filename in
  try testfun (Format.formatter_of_out_channel chan)
  with exn -> (close_out chan; raise exn)

let () =
  if Array.length Sys.argv = 2 then
    runtest Sys.argv.(1)
  else
    failwith "Usage: author TESTNAME"
