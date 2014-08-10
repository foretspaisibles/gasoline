(* Main -- Test the Getopt module

Author: Michael Grünewald
Date: Sun Aug 10 14:49:52 CEST 2014

Gasoline (https://github.com/michipili/gasoline)
This file is part of Gasoline

Copyright © 2014 Michael Grünewald

This file must be used under the terms of the CeCILL-B.
This source file is licensed as described in the file COPYING, which
you should have received as part of this distribution. The terms
are also available at
http://www.cecill.info/licences/Licence_CeCILL-B_V1-en.txt *)
open Printf

let rest =
  ref []

let spec = Getopt.spec
  "[-avh][-i int][-f float][-b bool][-s string][--][rest]"
  "Test the Getopt module in Gasoline" [
    Getopt.flag 'a'
      (fun () -> printf "flag a\n") "The a flag.";
    Getopt.flag 'v'
      (fun () -> printf "flag v\n") "The v flag.";
    Getopt.bool 'b'
      (fun b -> printf "option b = %B\n" b) "The b option.";
    Getopt.char 'c'
      (fun c -> printf "option c = %C\n" c) "The c option.";
    Getopt.string 's'
      (fun s -> printf "option s = %S\n" s) "The s option.";
    Getopt.int 'i'
      (fun x -> printf "option i = %d\n" x) "The i option.";
    Getopt.float 'f'
      (fun x -> printf "option f = %f\n" x) "The f option.";
  ] (Getopt.queue rest) [
    Getopt.note "Short Note"
      "This is an example of a short note.";
    Getopt.note "Long Note"
      "This is an example of a very long note whose contents spans \
       over several lines.  It really has to be that long, so we \
       will write here as much silly text as needed.\n  \
       And we also need several pragraphs of irrelevant text, but \
       believe me, this has nothing to do with you, just with the example.";
  ]

let () =
  begin
    Getopt.parse_argv spec;
    List.iter (fun s -> printf "rest = %S\n" s) !rest
  end
