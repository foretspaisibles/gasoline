(* Gasoline_Plain_Daemon -- C-Stylish Unix Daemons

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
    ~name:"#gasoline_plain_daemon"
    ~description:"A component owning daemon options"
    ()

let run_in_foreground =
  Configuration.make_bool internal
    ~flag:'F' ~optarg:"true"
    "stay_in_foreground" false
    "Run program in the foreground, without detaching."

let redirect_to_file fd name =
  let fdfile = Unix.openfile name [ Unix.O_WRONLY; Unix.O_CREAT; Unix.O_APPEND ] 0o600 in
  Unix.dup2 fdfile fd;
  Unix.close fdfile

let redirect_to_subprocess fd command =
  let fdr, fdw = Unix.pipe () in
  let _pid = Unix.create_process "/bin/sh" [| "/bin/sh"; "-c"; command |]
      fdr Unix.stdout Unix.stderr
  in
  Unix.close fdr;
  Unix.dup2 fdw fd

let maybe_redirect fd = function
  | Some(spec) ->
      if String.length spec > 0 && (spec.[0] = '/' || spec.[0] = '.') then
        redirect_to_file fd spec
      else
        redirect_to_subprocess fd spec
  | None -> ()

let detach chdir stdout stderr =
  if Unix.fork () > 0 then exit 0;
  (match chdir with
   | Some(path) -> Unix.chdir path
   | None -> ());
  Unix.close Unix.stdin;
  maybe_redirect Unix.stdout stdout;
  maybe_redirect Unix.stderr stderr;
  ignore(Unix.setsid())


let run name usage description ?notes ?system_configuration ?chdir ?stdout ?stderr main =
  let maybe_important_file = function
    | Some(name) -> Some(Configuration.ImportantFile(name()))
    | None -> None
  in
  let actual_main rest =
    if not(run_in_foreground()) then detach chdir stdout stderr;
    main rest
  in
  Gasoline_Plain_Application.run
    ~configuration:Configuration.(
        List.fold_left (fun acc -> function
            | Some(conf) -> Merge(acc, conf)
            | None -> acc) Empty [
          Some(Command_line);
          Some(Environment);
          maybe_important_file system_configuration;
        ])
    name usage description ?notes actual_main
