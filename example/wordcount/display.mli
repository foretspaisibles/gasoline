(* Display -- Display word counts

Author: Michael Grünewald
Date: Sat Jan 25 19:28:02 CET 2014

Gasoline (https://github.com/michipili/gasoline)
This file is part of Gasoline

Copyright © 2014 Michael Grünewald

This file must be used under the terms of the CeCILL-B.
This source file is licensed as described in the file COPYING, which
you should have received as part of this distribution. The terms
are also available at
http://www.cecill.info/licences/Licence_CeCILL-B_V1-en.txt *)

type spec = {
  bytes: bool;
  lines: bool;
  chars: bool;
  words: bool;
  longest: bool;
}

val print : spec -> string -> Count.t -> unit
(** [print spec file stat] print the given statistics. *)
