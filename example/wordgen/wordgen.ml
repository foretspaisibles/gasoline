(* Wordgen -- Word generator

Author: Michael Grünewald
Date: Fri Nov  8 07:55:44 2013 CET

Gasoline (https://github.com/michipili/gasoline)
This file is part of Gasoline

Copyright © 2013 Michael Grünewald

This file must be used under the terms of the CeCILL-B.
This source file is licensed as described in the file COPYING, which
you should have received as part of this distribution. The terms
are also available at
http://www.cecill.info/licences/Licence_CeCILL-B_V1-en.txt *)

module Application =
  CApplication

exception Error

(* Software components *)

module Component_automaton =
struct

  let comp = {
    Application.Component.
    name = "automaton";
    version = "1.0";
    require = [];
    provide = [];
    description = "Automaton parameters";
    config_prefix = [];
    getopt_prefix = Some 'A';
  }

  module Configuration =
  struct
    open Application.Value
    open Application.Configuration

    let generate_n =
      make Int comp ~flag:'n'
        "number" 1
        "The number of generated words"

    let generate_min =
      make Int comp ~flag:'a'
        "generate_min" 5
        "The shortest length a generated word can have"

    let generate_max =
      make Int comp ~flag:'b'
        "generate_max" 12
        "The largest length a generated word can have"

    let length =
      make Int comp ~flag:'l'
        "length" 3
        "The length of a word part"
  end

  module Message =
  struct
    open Application.Value
    open Application.Message
    open Application.Classification

    let sink =
      Application.Component.sink comp

    let file_not_found name reason =
      send sink Error "${FILENAME:s}: cannot open (${REASON:s})"
	[ "FILENAME", make String name;
	  "REASON", make String reason;
	]

    let table_mismatch reason =
      send sink Error "table mismatch (${REASON:s})"
	[ "REASON", make String reason; ]
  end

  (* The functions compile and generate both use a local functor
     application to define operations that they export.

     It could be interesting to modify the implementation so that it
     uses one or more first class modules, the maintenance of the
     creation code could be shorter. *)

  let compile file =
    let module Parameter =
	struct
	  open Application.Configuration
	  open Configuration
	  let length = get length
	end
    in
    let module Table = Table.Make(Parameter) in
    try Table.to_persistant (Table.add_file Table.empty file)
    with Sys_error m -> (Message.file_not_found file m; raise Error)

  let generate p =
    let module Parameter =
	struct
	  open Application.Configuration
	  open Configuration
	  let generate_min = get generate_min
	  let generate_max = get generate_max
	  let length = get length
	  let n = get generate_n
	end
    in
    let module Table = Table.Make(Parameter) in
    let module Automaton = Automaton.Make(Parameter) in
    let a = Automaton.create
      (try Table.of_persistant p
       with Invalid_argument m -> (Message.table_mismatch m; raise Error))
    in
    let rec loop ax n =
      if n = 0
      then
	ax
      else
	loop ((Automaton.generate a)::ax) (pred n)
    in
    loop [] Parameter.n

  let () = Application.Component.register comp
end


module Component_library =
struct

  let comp = {
    Application.Component.
    name = "library";
    version = "1.0";
    require = [];
    provide = [];
    description = "Dictionary library";
    config_prefix = [];
    getopt_prefix = Some 'L';
  }

  module Configuration =
  struct
    open Application.Value
    open Application.Configuration

    let dump =
      make String comp ~flag:'d'
	"dump" "/var/wordgen"
	"The location of dictionary dumps"
  end

  module Message =
  struct
    open Application.Value
    open Application.Message
    open Application.Classification

    let sink =
      Application.Component.sink comp

    let file_not_found name reason =
      send sink Error "${FILENAME:s}: cannot open (${REASON:s})" [
	"FILENAME", make String name;
	"REASON", make String reason;
      ]

    let file_not_saved name reason =
      send sink Error "${FILENAME:s}: cannot save (${REASON:s})" [
	"FILENAME", make String name;
	"REASON", make String reason;
      ]
  end

  let library () =
    Library.make
      []
      (Application.Configuration.get Configuration.dump)

  let infer_name file =
    Filename.concat (Application.Configuration.get Configuration.dump) (file ^ ".wordgen")

  let load file =
    try Library.load (library()) (infer_name file)
    with
    | Not_found -> (Message.file_not_found file "Not found"; raise Error)
    | Sys_error m -> (Message.file_not_found file m; raise Error)

  let save file p =
    try Library.save (library()) file p
    with Sys_error m -> (Message.file_not_saved file m; raise Error)

  let list () =
    Library.list (library())

  let () = Application.Component.register comp
end



type operation =
| List
| Help
| Compile of string
| Generate of string

let operation =
  ref Help

let help () =
  Application.help ()

let usage mesg =
  Application.usage mesg

let list () =
  List.iter print_endline (Component_library.list ())

let compile file =
  Component_library.save
    (Component_library.infer_name file)
    (Component_automaton.compile file)

let generate dump =
  List.iter print_endline
    (Component_automaton.generate
       (Component_library.load dump))

let main arglist =
  if arglist <> [] then
    usage "spurious argument"
  else
    match !operation with
    | Help -> help ()
    | List -> list ()
    | Compile file -> compile file
    | Generate dump -> generate dump

let () =
  let open Application.Value in
  let open Application.Getopt in
  Application.run "wordgen"
    "[-h]"
    "Generate words imitating a dictionary"
    ~configuration:Application.Configuration.Command_line
    ~options:[
      flag 'h' (fun () -> operation := Help)
	"Display a cheerful help message and exit.";
      flag 'L' (fun () -> operation := List)
	"List the content of the dictionary library.";
      make String 'C' (fun s -> operation := Compile s)
	"FILE\n\
         Compile FILE and dump the result in the dictionary library.";
      make String 'G' (fun s -> operation := Generate s)
	"FILE\n\
         Generate words using dumped data FILE.";
    ] main
