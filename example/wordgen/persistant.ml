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

let buffer_sz =		(* Initial buffer size *)
  1000

type t =
  string

let with_open openfile closefile file f =
  let c = openfile file in
  try let answer = f c in (closefile c; answer)
  with exn -> (closefile c; raise exn)

let with_open_out =
  with_open open_out_bin close_out

let with_open_in =
  with_open open_in_bin close_in

let save file data =
  with_open_out file (fun c -> output_string c data)

let in_channel_contents c =
  let b = Buffer.create buffer_sz in
  try while true do
	Buffer.add_char b (input_char c)
      done;
      failwith "Persistant.in_channel_contents"
  with End_of_file -> Buffer.contents b

let load file =
  with_open_in file in_channel_contents
