(* CConfiguration -- Configuration for C-Stylish applications

Author: Michael Grünewald
Date: Sun May 12 10:46:09 CEST 2013

Copyright © 2013 Michael Grünewald

This file must be used under the terms of the CeCILL-B.
This source file is licensed as described in the file COPYING, which
you should have received as part of this distribution. The terms
are also available at
http://www.cecill.info/licences/Licence_CeCILL-B_V1-en.txt *)
(** Configuration for C-Stylish applications. *)


include module type of Configuration

(** Create a boolean configuration item. *)
val key_bool : string list -> string -> bool -> string -> bool key

(** Create a char configuration key. *)
val key_char : string list -> string -> char -> string -> char key

(** Create an integer configuration key. *)
val key_int : string list -> string -> int -> string -> int key

(** Create a float configuration key. *)
val key_float : string list -> string -> float -> string -> float key

(** Create a string configuration key. *)
val key_string : string list -> string -> string -> string -> string key
