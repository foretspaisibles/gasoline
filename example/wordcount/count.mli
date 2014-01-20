(* Count -- Count words

Author: Michael Grünewald
Date: Sat Jan 25 18:28:20 CET 2014

Gasoline (https://github.com/michipili/gasoline)
This file is part of Gasoline

Copyright © 2014 Michael Grünewald

This file must be used under the terms of the CeCILL-B.
This source file is licensed as described in the file COPYING, which
you should have received as part of this distribution. The terms
are also available at
http://www.cecill.info/licences/Licence_CeCILL-B_V1-en.txt *)

(** Count words. *)

(** The type of statistics. *)
type t = {
  bytes: int;
  lines: int;
  chars: int;
  words: int;
  longest: int;
}

val from_stream : char Stream.t -> t
(** Statistics associated to a stream. *)

val from_in_channel : in_channel -> t
(** Statistics associated to an input channel. *)

val from_file : string -> t
(** Statistics associated to a file.

@raise Sys_error if the file cannot be read. *)

val total : t list -> t
(** Statistics summarising a list of statistics.

In the summary, the fields [bytes], [lines], [chars] and [words] are
the sum of the corresponding fields for the elements in the list.  The
field [longest] is the maximum of the values of the corresponding
field for the elements in the list.

The statistics summarising an empty list are all 0. *)
