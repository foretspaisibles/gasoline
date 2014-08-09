(* test_html_simple_2 -- Author kit

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
open WHTML

let document contents =
  html [ body contents ]

let a_h1 = h1 (u"My First Heading")

let a_plong =  p [c"This rather long paragraph \
  is used to demonstrate how text is folded within a regular paragraph."]

let testsimple contents =
  WSGML.print (document contents)

let () = testsimple [ a_h1; a_plong ]
