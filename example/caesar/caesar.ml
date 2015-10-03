(* Caesar -- Caesar's cipher

Gasoline (https://github.com/michipili/gasoline)
This file is part of Gasoline

Copyright © 2013–2015 Michael Grünewald

This file must be used under the terms of the CeCILL-B.
This source file is licensed as described in the file COPYING, which
you should have received as part of this distribution. The terms
are also available at
http://www.cecill.info/licences/Licence_CeCILL-B_V1-en.txt *)
module Application =
  Gasoline_Plain_Filter

let caesar =
  let downcase = "abcdefghijklmnopqrstuvwxyz" in
  let upcase = "ABCDEFGHIJKLMNOPQRSTUVWXYZ" in
  fun c ->
    if String.contains downcase c then
      downcase.[(String.index downcase c + 13) mod 26]
    else if String.contains upcase c then
      upcase.[(String.index upcase c + 13) mod 26]
    else
      c

let main in_channel out_channel =
  let output_char_and_flush channel c =
    output_char channel c;
    if c = '\n' then flush channel
  in
  while true do
    output_char_and_flush out_channel (caesar (input_char in_channel))
  done

let () =
  Application.run "caesar" "[-i suffix]"
    "Caesar's cipher, useful to obfuscate spoilers or puzzle solutions"
    main
