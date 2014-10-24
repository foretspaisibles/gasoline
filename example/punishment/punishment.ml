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
  CApplication

module Monolith =
struct
  let comp = {
    Application.Component.
    name = "monolith";
    version = "1.0";
    require = [];
    provide = [];
    description = "The monolithic component of our application";
    config_prefix = [];
    getopt_prefix = None;
  }

  module Configuration =
  struct
    open Application.Value
    open Application.Configuration

    let number =
      make Int comp ~flag:'n' ~env:"NUMBER"
	"number" 100
	"The number of times our punishment must be printed"

    let paragraph =
      make String comp ~flag:'p' ~env:"PARAGRAPH"
	"paragraph" "I must not talk in class"
	"The paragraph we must copy as punishment"

    let configfile =
      make String comp ~flag:'c' ~env:"CONFIGFILE"
        "configfile" ""
	"The name of an additional configuration file to read"
  end


  module Message =
  struct
    open Application.Value
    open Application.Message
    open Application.Classification

    let sink =
      Application.Component.sink comp

    let plan number paragraph =
      send sink Info "I am about to perform a punishment, writing \
                      ${NUMBER:d} times ${PARAGRAPH:S}." [
        "NUMBER", make Int number;
        "PARAGRAPH", make String paragraph;
      ]

    let we_need_time () =
      send sink Warning "This could last more than a few seconds." []
  end

  let perform () =
    let open Application.Configuration in
    let number = get Configuration.number in
    let paragraph = get Configuration.paragraph in
    let really_big_number = 32_000 in
    Message.plan number paragraph;
    if number > really_big_number then
      Message.we_need_time ();
    for i = 1 to number do
      Printf.printf "%4d %s\n" i paragraph
    done

  let () = Application.Component.register comp
end

let main lst =
  match lst with
  | [] -> Monolith.perform ()
  | _ -> Application.usage "spurious argument"

let () =
  let configuration =
    let open Application.Configuration in
    Merge(
	Merge(Environment, Command_line),
	RandomFile(Monolith.Configuration.configfile))
  in
  Application.run
    "punishment"
    "[-n number] [-p paragraph] [-c configfile]"
    "I must not talk in class."
    ~configuration
    main
