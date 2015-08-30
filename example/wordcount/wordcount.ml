(* Wordcount -- Counting words

Author: Michael Grünewald
Date: Mon Jan 20 23:12:00 CET 2014

Gasoline (https://github.com/michipili/gasoline)
This file is part of Gasoline

Copyright © 2014 Michael Grünewald

This file must be used under the terms of the CeCILL-B.
This source file is licensed as described in the file COPYING, which
you should have received as part of this distribution. The terms
are also available at
http://www.cecill.info/licences/Licence_CeCILL-B_V1-en.txt *)

module Application =
  Gasoline_Plain_Application

exception Error

(* Word counter *)

module Component_count =
struct

  let comp =
    Application.Component.make
      ~name:"count"
      ~description:"Count words"
      ()

  module Message =
  struct
    open Printf
    let cannot_open name reason =
      eprintf  "Error: %s: cannot open (%s)" name reason
  end

  let count_channel c =
    Count.from_in_channel c

  let count_file name =
    try Count.from_file name
    with Sys_error reason -> (Message.cannot_open name reason; raise Error)
end


module Component_display =
struct

  let comp =
    Application.Component.make
      ~name:"display"
      ~description:"Display word counts"
      ()

  module Configuration =
  struct
    open Application.Configuration

    let bytes =
      make_bool comp ~flag:'c' ~optarg:"true"
        "bytes" false
        "The number of bytes in each input file is written to the standard \
         output.  This will cancel out any prior usage of the -m option."

    let lines =
      make_bool comp ~flag:'l' ~optarg:"true"
        "lines" false
        "The number of lines in each input file is written to the standard \
         output."

    let chars =
      make_bool comp ~flag:'m' ~optarg:"true"
        "chars" false
        "The number of characters in each input file is written to the \
         standard output. If the current locale does not support multibyte \
         characters, this is equivalent to the -c option. This will cancel \
         out any prior usage of the -c option."

    let words =
      make_bool comp ~flag:'w' ~optarg:"true"
        "words" false
        "The number of words in each input file is written to the standard \
         output."

    let longest =
      make_bool comp ~flag:'L' ~optarg:"true"
        "longest" false
        "The number of characters in the longest input line is written to \
         the standard output.  When more then one file argument is specified, \
         the longest input line of all files is reported as the \
         value of the final “total”."
  end

  let spec () =
    let open Configuration in
    let all () =
      not (chars () || bytes () || words ())
    in
    Display.{
      chars = chars () || all ();
      bytes = bytes () || all ();
      lines = lines () || all ();
      words = words () || all ();
      longest = longest ();
    }

  let print_count filename stat =
    Display.print (spec()) filename stat

  let print_summary lst =
    Display.print (spec()) "total" (Count.total lst)
end


let queue_elements q =
  Queue.fold (fun a x -> x :: a) [] q

type operation =
| Count
| Help

let operation =
  ref Count

let count_channel c =
  let stat = Component_count.count_channel c in
  Component_display.print_count "" stat

let count_file file =
  let stat = Component_count.count_file file in
  Component_display.print_count file stat

(* TODO: Handle exceptions *)
let count_files lst =
  let q = Queue.create () in
  let loop file =
    try
      let stat = Component_count.count_file file in
      Component_display.print_count file stat;
      Queue.add stat q
    with exn -> raise exn
  in
  List.iter loop lst;
  if List.length lst > 1 then
    Component_display.print_summary (queue_elements q)

let count files =
  match files with
  | [] -> count_channel stdin
  | _ -> count_files files

let main files =
  match !operation with
  | Help -> ignore ()
  | Count -> count files

let () = Application.run "wordcount"
    "[-chlmwL] [file ...]"
  "Word, line, character and byte count"
  ~configuration:Application.Configuration.Command_line
  main
