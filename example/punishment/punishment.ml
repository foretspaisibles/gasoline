(* Punishment -- I must not talk in class

Author: Michael Grünewald
Date: Sun Sep 21 01:22:31 CEST 2014

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

module Monolith =
struct
  let comp = Application.Component.make
      ~name:"monolith"
      ~description:"The monolithic component of our application"
      ()

  module Configuration =
  struct
    open Application.Configuration

    let number =
      make_int comp ~flag:'n' ~env:"NUMBER"
        "number" 100
        "The number of times our punishment must be printed"

    let paragraph =
      make_string comp ~flag:'p' ~env:"PARAGRAPH"
        "paragraph" "I must not talk in class"
        "The paragraph we must copy as punishment"

    let configfile =
      make_string comp ~flag:'c' ~env:"CONFIGFILE"
        "#configfile" ""
        "The name of an additional configuration file to read"
  end

  let perform () =
    let open Application.Configuration in
    let number = Configuration.number () in
    let paragraph = Configuration.paragraph () in
    for i = 1 to number do
      Printf.printf "%4d %s\n" i paragraph
    done
end

let main lst =
  match lst with
  | [] -> Monolith.perform ()
  | _ -> failwith "spurious argument"

let () =
  let configuration =
    let open Application.Configuration in
    Merge(
        Merge(Environment, Command_line),
        UserFile(["monolith"], "#configfile"))
  in
  Application.run
    "punishment"
    "[-n number] [-p paragraph] [-c configfile]"
    "I must not talk in class."
    ~configuration
    main
