(* test_html_simple_3 -- Author kit

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
open Gasoline_Unicode
open Gasoline_Author_HTML

let document contents =
  html [ body contents ]

let a_h1attr = h1
  ~id:"this-header-as-such-a-long-identifier-it-has-indeeed"
  ~cls:"this-class-as-quite-along-identifier-as-well"
  (u"My First Heading")

let a_p = p [c"My first paragraph."]

let testsimple contents =
  Gasoline_Author_SGML.print (document contents)

let () = testsimple [ a_h1attr; a_p ]
