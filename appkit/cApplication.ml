(* CApplication -- C-Stylish Applications

Author: Michael Grünewald
Date: Sat Dec 28 13:05:16 CET 2013

Gasoline (https://github.com/michipili/gasoline)
This file is part of Gasoline

Copyright © 2013 Michael Grünewald

This file must be used under the terms of the CeCILL-B.
This source file is licensed as described in the file COPYING, which
you should have received as part of this distribution. The terms
are also available at
http://www.cecill.info/licences/Licence_CeCILL-B_V1-en.txt *)
module Value = CType.Value


module Message =
struct

  type classification =
  | Debug
  | Info
  | Notice
  | Warning
  | Error
  | Critical
  | Alert
  | Emergency

  type sink =
    unit

  let send sink clas id binding =
    failwith "CApplication.Message.send: Not implemented"

end


module Configuration =
struct
  type 'a t = {
    kind: 'a Value.kind;
  }

  let make kind component ?flag ?env ?shy name default description =
    failwith "CApplication.Configuration.make: Not implemented"

  let get item =
    failwith "CApplication.Configuration.get: Not implemented"

  let set item value =
    failwith "CApplication.Configuration.set: Not implemented"
end



module Component =
struct

  type t = {
    name: string;
    version: string;
    require: string list;
    description: string;
    config_prefix: string list;
    getopt_prefix: char option;
  }

  let sink _ =
    failwith "CApplication.Component.sink: Not implemented"

end

module Getopt =
struct

  type t =
    unit

  type spec =
    unit

  let flag c callback description =
    failwith "CApplication.Getopt.flag: Not implemented"

  let make kind c callback description =
    failwith "CApplication.Getopt.make: Not implemented"

  let note title text =
    failwith "CApplication.Getopt.note: Not implemented"

  let spec usage description getopt_list rest =
    failwith "CApplication.Getopt.rest: Not implemented"

  let help_message spec =
    failwith "CApplication.Getopt.rest: Not implemented"

  let help spec =
    failwith "CApplication.Getopt.help: Not implemented"

end

let init () =
  failwith "CApplication.init: Not implemented"

let getopt spec =
  failwith "CApplication.getopt: Not implemented"

let getopt_list () =
  failwith "CApplication.getopt_list: Not implemented"
