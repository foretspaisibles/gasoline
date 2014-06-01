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
  CApplication

exception Error

(* Word counter *)

module Component_count =
struct

  let comp = {
    Application.Component.
    name = "count";
    version = "1.0";
    require = [];
    description = "Count words";
    config_prefix = [];
    getopt_prefix = None;
  }

  module Message =
  struct
    open Application.Value
    open Application.Message

    let sink =
      Application.Component.sink comp

    let cannot_open name reason =
      send sink Error "${FILENAME}: cannot open (${REASON})"
	[ "FILENAME", make String name;
	  "REASON", make String reason;
	]
  end


  let count_channel c =
    Count.from_in_channel c


  let count_file name =
    try Count.from_file name
    with Sys_error reason -> (Message.cannot_open name reason; raise Error)

end


module Component_display =
struct

  let comp = {
    Application.Component.
    name = "display";
    version = "1.0";
    require = [];
    description = "Display word counts";
    config_prefix = [];
    getopt_prefix = None;
  }

  module Configuration =
  struct
    open Application.Value
    open Application.Configuration

    let bytes =
      make Bool comp ~flag:'c'
	"bytes" true
	"The number of bytes in each input file is written to the standard \
         output.  This will cancel out any prior usage of the -m option."

    let lines =
      make Bool comp ~flag:'l'
	"lines" true
	"The number of lines in each input file is written to the standard \
         output."

    let chars =
      make Bool comp ~flag:'m'
	"chars" true
	"The number of characters in each input file is written to the \
         standard output. If the current locale does not support multibyte \
         characters, this is equivalent to the -c option. This will cancel \
         out any prior usage of the -c option."

    let words =
      make Bool comp ~flag:'w'
	"words" true
	"The number of words in each input file is written to the standard \
         output."

    let longest =
      make Bool comp ~flag:'L'
	"longest" true
	"The number of characters in the longest input line is written to \
         the standard output.  When more then one file argument is specified, \
         the longest input line of all files is reported as the \
         value of the final “total”."
  end


  (* This module is only a placeholder, as it is not actually used. *)
  module Message =
  struct
    open Application.Value
    open Application.Message

    let sink =
      Application.Component.sink comp

  end

  let spec () =
    let open Application.Configuration in
    let open Configuration in {
      Display.
      chars = get chars;
      bytes = get bytes;
      lines = get lines;
      words = get words;
      longest = get longest;
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

let args =
  Queue.create ()

let push name =
  Queue.add name

let getopt_spec () =
  let open Application in
  let open Value in
  Getopt.spec "wordcount [-h][-clmwL]"
    "Word, line, character, and byte count"
    (getopt_list () @ [
      Getopt.flag 'h' (fun () -> operation := Help)
	"Display a cheerful help message and exit.";
    ]) ignore

let help () =
  let open Application in
  Getopt.help (getopt_spec())

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
  Component_display.print_summary (queue_elements q)


let main () =
  Application.init ();
  Application.getopt (getopt_spec());
  begin
  end;
  match !operation with
  | Help -> help ()
  | Count -> ignore ()

(* TODO: Implement the flag logic

The flag logic is the one described in `wc(1)`.

This involves preventing most configuration values from being edited
from the command line, using callbacks instead. *)

let () =
  try (main (); exit 0)
  with Error -> exit 1
