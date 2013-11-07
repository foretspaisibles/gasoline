(* Persistant -- Persistant data

Author: Michael Grünewald
Date: Fri Nov  8 07:54:03 2013 CET

Gasoline (https://github.com/michipili/gasoline)
This file is part of Gasoline

Copyright © 2013 Michael Grünewald

This file must be used under the terms of the CeCILL-B.
This source file is licensed as described in the file COPYING, which
you should have received as part of this distribution. The terms
are also available at
http://www.cecill.info/licences/Licence_CeCILL-B_V1-en.txt *)

type t =
  External.sexp


let with_open_out file f =
  let c = open_out file in
  try let answer = f c in (close_out c; answer)
  with exn -> (close_out c; raise exn)

let save file data =
  failwith "Not implemented"

let load file =
  failwith "Not implemented"
