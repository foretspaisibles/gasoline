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
  Gasoline_Plain_Application

exception Error

(* Software components *)

module Component_Automaton =
struct

  let comp =
    Application.Component.make
      ~name:"automaton"
      ~description:"Automaton parameters"
      ~getopt_prefix:'A'
      ()

  module Configuration =
  struct
    open Application.Configuration

    let generate_n =
      make_int comp ~flag:'n'
        "number" 1
        "The number of generated words"

    let generate_min =
      make_int comp ~flag:'a'
        "generate_min" 5
        "The shortest length a generated word can have"

    let generate_max =
      make_int comp ~flag:'b'
        "generate_max" 12
        "The largest length a generated word can have"

    let length =
      make_int comp ~flag:'l'
        "length" 3
        "The length of a word part"
  end

  module Message =
  struct
    open Printf

    let file_not_found name reason =
      eprintf "Error: %s: cannot open (%s)\n" name reason

    let table_mismatch reason =
      eprintf "Error: table mismatch (%s)\n" reason
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
          let length = length ()
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
          let generate_min = generate_min ()
          let generate_max = generate_max ()
          let length = length ()
          let n = generate_n ()
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
end


module Component_Library =
struct

  let comp =
    Application.Component.make
      ~name:"library"
      ~description:"Dictionary library"
      ~getopt_prefix:'L'
      ()

  module Configuration =
  struct
    open Application.Configuration

    let dump =
      make_string comp ~flag:'d'
        "dump" "/var/wordgen"
        "The location of dictionary dumps"
  end

  module Message =
  struct
    open Printf

    let file_not_found name reason =
      eprintf "Error: %s: cannot open (%s)\n" name reason

    let file_not_saved name reason =
      eprintf "Error: %s: cannot save (%s)\n" name reason
  end

  let library () =
    Library.make [] (Configuration.dump ())

  let infer_name file =
    Filename.concat (Configuration.dump ()) (file ^ ".wordgen")

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
end


module Component_Main =
struct

  let comp =
    Application.Component.make
      ~name:"main"
      ~description:"Main component"
      ()

  module Configuration =
  struct
    open Application.Configuration

    type operation =
      | List
      | Usage
      | Compile of string
      | Generate of string

    let list =
      make_bool comp ~flag:'L' ~optarg:"true"
        "#list" false
        "List the content of the dictionary library."

    let compile =
      make_string comp ~flag:'C'
        "#compile" ""
        "FILE\n\
         Compile FILE and dump the result in the dictionary library."

    let generate =
      make_string comp ~flag:'G'
        "#generate" ""
        "FILE\n\
         Generate words using dumped data FILE."

    let operation () =
      match list (), compile (), generate () with
      | true, "", "" -> List
      | false, file, "" -> Compile(file)
      | false, "", file -> Generate(file)
      | _ -> Usage
  end

  let list () =
    List.iter print_endline (Component_Library.list ())

  let compile file =
    Component_Library.save
      (Component_Library.infer_name file)
      (Component_Automaton.compile file)

  let generate dump =
    List.iter print_endline
      (Component_Automaton.generate
         (Component_Library.load dump))

  let help () =
    failwith "Not implemented"

  let usage mesg =
    failwith "Not implemented"

  let run arglist =
    if arglist <> [] then
      usage "spurious argument"
    else
      match Configuration.operation () with
      | Configuration.Usage -> usage ""
      | Configuration.List -> list ()
      | Configuration.Compile file -> compile file
      | Configuration.Generate dump -> generate dump
end

let () =
  Application.run "wordgen"
    "[-h]"
    "Generate words imitating a dictionary"
    ~configuration:Application.Configuration.Command_line
    Component_Main.run
