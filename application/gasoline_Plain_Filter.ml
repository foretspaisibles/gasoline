(* Gasoline_Plain_Filter -- C-Stylish Unix Filters

Gasoline (https://github.com/michipili/gasoline)
This file is part of Gasoline

Copyright © 2013–2015 Michael Grünewald

This file must be used under the terms of the CeCILL-B.
This source file is licensed as described in the file COPYING, which
you should have received as part of this distribution. The terms
are also available at
http://www.cecill.info/licences/Licence_CeCILL-B_V1-en.txt *)
module Component =
  Gasoline_Plain_Application.Component

module Configuration =
  Gasoline_Plain_Application.Configuration

let internal =
  Component.make
    ~name:"#gasoline_plain_filter"
    ~description:"A component owning filter options"
    ()

let inplace =
  Configuration.make (fun suffix -> Some(suffix)) internal
    ~flag:'i'
    "inplace_suffix" None
    "The suffix used to rename file when editing inplace."

let run name usage description ?notes main =
  let with_filein f name =
    let channel = open_in name in
    try (f channel; close_in channel)
    with
    | exn -> (close_in channel; raise exn)
  in
  let with_inplace suffix f name =
    Sys.rename name (name^suffix);
    let in_channel, out_channel =
      open_in (name^suffix), open_out name
    in
    try (f in_channel out_channel; close_in in_channel; close_out out_channel)
    with
    | exn -> (close_in in_channel; close_out out_channel; raise exn)
  in
  let safe_main in_channel out_channel =
    try main in_channel out_channel
    with End_of_file -> ()
  in
  let actual_main argv =
    match argv, inplace () with
    | [], None -> safe_main Pervasives.stdin Pervasives.stdout
    | [], Some(_) -> failwith "Usage: Cannot edit stdin inplace."
    | _, None ->
        List.iter (with_filein (fun channel -> safe_main channel stdout)) argv
    | _, Some("") -> failwith "Usage: Cannot edit file inplace without a proper suffix."
    | _, Some(suffix) ->
        List.iter (with_inplace suffix (fun in_channel out_channel -> safe_main in_channel out_channel)) argv
  in
  Gasoline_Plain_Application.run
    ~configuration:Gasoline_Plain_Application.Configuration.(
        Merge(Command_line, Environment))
    name usage description ?notes actual_main
