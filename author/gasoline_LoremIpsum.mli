(* Gasoline_LoremIpsum -- Text generator

Author: Michael Grünewald
Date: Mon  9 Mar 2009 18:47:07 CET

Gasoline (https://github.com/michipili/gasoline)
This file is part of Gasoline

Copyright © 2008–2014 Michael Grünewald

This file must be used under the terms of the CeCILL-B.
This source file is licensed as described in the file COPYING, which
you should have received as part of this distribution. The terms
are also available at
http://www.cecill.info/licences/Licence_CeCILL-B_V1-en.txt *)
(** Lorem Ipsum *)

val title : unit -> string
(** Make a string suitable to be used as a title. *)

val sentence : unit -> string
(** Make a sentence. *)

val paragraph : unit -> string
(** Make a string suitable to be used as a paragraph. *)

val text : unit -> string list
(** Make a list of string suitable to be used as a list of paragraphs. *)

val code : unit -> string
(** Make a string suitable to be used a bunch of code. *)

val shell : unit -> string * string
(** Make a string suitable to be used as a shell interaction sample, with the
user command and the output. *)
