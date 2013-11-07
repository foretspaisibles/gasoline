(* Library -- Dictionary library

Author: Michael Grünewald
Date: Fri Nov  8 07:52:59 2013 CET

Gasoline (https://github.com/michipili/gasoline)
This file is part of Gasoline

Copyright © 2013 Michael Grünewald

This file must be used under the terms of the CeCILL-B.
This source file is licensed as described in the file COPYING, which
you should have received as part of this distribution. The terms
are also available at
http://www.cecill.info/licences/Licence_CeCILL-B_V1-en.txt *)

(** Dictionary library. *)

type t
(** The abstract type of dictionary libraries. *)

val make : string list -> string -> t
(** [make path dump] create a dictionary library that can load
dictionary data from the given [path] and [dump] directories, and can
write data to the [dump] directory. *)

val list : t -> string list
(** List available dictionaries. *)

val load : t -> string -> Persistant.t
(** Load the given dictionary.

@raise Not_found if the file cannot be found in the library.
@raise Sys_error if the file cannot be accessed. *)

val save : t -> string -> Persistant.t -> unit
(** Save the given dictionary. *)
