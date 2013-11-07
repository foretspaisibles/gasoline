(* Persistant -- Persistant data

Author: Michael Grünewald
Date: Fri Nov  8 07:54:03 2013 CET

Gasoline (https://github.com/michipili/gasoline)
This file is part of Gasoline

Copyright © 2013 Michael Grünewald

This file must be used under the terms of the CeCILL-B.
This source file is licensed as described in the file COPYING, which
you should have received as part of this distribution. The terms
are also available at
http://www.cecill.info/licences/Licence_CeCILL-B_V1-en.txt *)

(** Persistant data. *)
open External

(** Persistant data. *)

type t = sexp
(** The type of persistant data. *)

val save : string -> t -> unit
(** [save file data] write [data] in [file], so that it later can be
retrieved with [load] file. *)

val load : string -> t
(** [load file] retrieve persistant data that was previously written
in [file] with [save]. *)
