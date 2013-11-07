(* Library -- Dictionary library

Author: Michael Grünewald
Date: Fri Nov  8 07:52:59 2013 CET

Gasoline (https://github.com/michipili/gasoline)
This file is part of Gasoline

Copyright © 2013 Michael Grünewald

This file must be used under the terms of the CeCILL-B.
This source file is licensed as described in the file COPYING, which
you should have received as part of this distribution. The terms
are also available at
http://www.cecill.info/licences/Licence_CeCILL-B_V1-en.txt *)

type t = {
  path: string list;
  dump: string;
}

let make path dump = {
  path;
  dump;
}

let list _ =
  []

let rec lookup ax file =
  match ax with
  | [] -> raise Not_found
  | h :: t -> maybe t h file
and maybe t h file =
  let path = Filename.concat h file in
  if Sys.file_exists path then path else lookup t file

let load lib file =
  Persistant.load (
    lookup (lib.dump :: lib.path) file
  )

let save lib file data =
  Persistant.save
    (Filename.concat lib.dump file)
    data
