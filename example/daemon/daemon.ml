(* Daemon -- A cute little daemon

Gasoline (https://github.com/michipili/gasoline)
This file is part of Gasoline

Copyright © 2013–2015 Michael Grünewald

This file must be used under the terms of the CeCILL-B.
This source file is licensed as described in the file COPYING, which
you should have received as part of this distribution. The terms
are also available at
http://www.cecill.info/licences/Licence_CeCILL-B_V1-en.txt *)
module Application =
  Gasoline_Plain_Daemon

let main rest =
  let rec loop n =
    if n > 0 then begin
      ignore(Sys.command "date");
      Unix.sleep 1;
      loop (pred n)
    end else
      exit 0
  in
  loop 10

let () =
  Application.run "daemon" "[-F]"
(*    ~stdout:"./daemon.log" *)
    ~stdout:"logger -i -s"
    "A toy daemon, sitting idle"
    main
