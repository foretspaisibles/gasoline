(* Count -- Count words

Author: Michael Grünewald
Date: Sat Jan 25 18:28:20 CET 2014

Gasoline (https://github.com/michipili/gasoline)
This file is part of Gasoline

Copyright © 2014 Michael Grünewald

This file must be used under the terms of the CeCILL-B.
This source file is licensed as described in the file COPYING, which
you should have received as part of this distribution. The terms
are also available at
http://www.cecill.info/licences/Licence_CeCILL-B_V1-en.txt *)

(* We use a simple method to count words in the text: we just remebmer
if we were inside a word or not, and when a character makes switch
from within word to non-word, then we add one. *)

let is_wordchar c =
  String.contains "abcedefghijklmnopqrstuvwxyz\
                   ABCEDEFGHIJKLMNOPQRSTUVWXYZ" c

type t = {
  bytes: int;
  lines: int;
  chars: int;
  words: int;
  longest: int;
}

let from_stream s =
  let bytes = ref 0 in
  let lines = ref 0 in
  let words = ref 0 in
  let longest = ref 0 in
  let current = ref 0 in
  let in_word = ref false in
  try
    while true do
      match Stream.next s with
      | '\n' -> ( longest := max !longest !current;
		  current := 0;
		  incr bytes;
		  incr lines;
                  if !in_word then incr words;
                  in_word := false; )
      | c -> ( incr current;
	       incr bytes;
	       if !in_word && not(is_wordchar c) then incr words;
	       in_word := is_wordchar c; )
    done;
    failwith "Count.from_stream"
  with
  | Stream.Failure -> ( if !in_word then incr words;
			{ bytes = !bytes;
			  lines = !lines;
			  chars = !bytes;
			  words = !words;
			  longest = !longest; } )


let from_in_channel c =
  from_stream (Stream.of_channel c)

let from_file name =
  let c = open_in name in
  let answer =
    (try from_in_channel c with exn -> close_in c; raise exn)
  in
  (close_in c; answer)

let total lst =
  let add s1 s2 = {
    bytes = s1.bytes + s2.bytes;
    lines = s1.lines + s2.lines;
    chars = s1.chars + s2.chars;
    words = s1.words + s2.words;
    longest = max s1.longest s2.longest;
  } in
  let zero = {
    bytes = 0;
    lines = 0;
    chars = 0;
    words = 0;
    longest = 0;
  } in
  List.fold_right add lst zero
