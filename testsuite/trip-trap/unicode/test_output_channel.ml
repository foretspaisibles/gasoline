(* test_output_channel -- Unicode

Author: Michael Grünewald
Date: Mon Jun  2 11:51:10 CEST 2014

Gasoline (https://github.com/michipili/gasoline)
This file is part of Gasoline

Copyright © 2014 Michael Grünewald

This file must be used under the terms of the CeCILL-B.
This source file is licensed as described in the file COPYING, which
you should have received as part of this distribution. The terms
are also available at
http://www.cecill.info/licences/Licence_CeCILL-B_V1-en.txt *)

open Unicode

let buffer_sz = 32

let data="\
À ces mots, le roi courut à la pucelle, et ils s’embrassèrent et \
s’accolèrent comme jeunes gens qui s’aiment. Puis, s’étant couvert \
le chef d’un heaume que Guenièvre lui avait donné, Artus monta à cheval,
et sa troupe se mit en marche au petit pas, lances basses et gonfanons ployés."

module Make(Stdio:UChannel_stdio.S) =
struct

  let main () =
    let b = UBuffer.create buffer_sz in
    let maybe_issue () =
      if UBuffer.length b > 0 then begin
	Stdio.print_ustring (UBuffer.contents b);
	Stdio.print_newline();
	UBuffer.clear b;
      end
    in
    let loop u =
      if UInformation.is_alpha u then
	UBuffer.add_uchar b u
      else
	maybe_issue()
    in
    UString.iter loop (u8 data)
end

module Stdio_parameter =
struct
  let stdin = Pervasives.stdin
  let stdout = Pervasives.stdout
  let stderr = Pervasives.stderr
  let stdin_enc = Encoding.utf8
  let stdout_enc = Encoding.utf8
  let stderr_enc = Encoding.utf8
end

let main () =
  let module Stdio = UChannel_stdio.Make(Stdio_parameter) in
  let module Application = Make(Stdio) in
  Application.main()

let () = main ()
