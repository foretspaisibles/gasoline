(* Display -- Display word counts

Author: Michael Grünewald
Date: Sat Jan 25 19:28:02 CET 2014

Gasoline (https://github.com/michipili/gasoline)
This file is part of Gasoline

Copyright © 2014 Michael Grünewald

This file must be used under the terms of the CeCILL-B.
This source file is licensed as described in the file COPYING, which
you should have received as part of this distribution. The terms
are also available at
http://www.cecill.info/licences/Licence_CeCILL-B_V1-en.txt *)
open Printf

type spec = {
  bytes: bool;
  lines: bool;
  chars: bool;
  words: bool;
  longest: bool;
}

let print_int n =
  printf " %7d" n

let print_filename s =
  if not(s = "") then printf " %s" s

let print_newline () =
  printf "\n"

let print spec filename stat =
  if spec.longest then (
    print_int stat.Count.longest
  ) else (
    List.iter (fun (flag,n) -> if flag then print_int n) [
      spec.lines, stat.Count.lines;
      spec.words, stat.Count.words;
      spec.bytes, stat.Count.bytes;
      spec.chars, stat.Count.chars;
    ];
  );
  print_filename filename;
  print_newline ()
