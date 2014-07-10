(* CValue -- Dynamically typed values for plain applications

Author: Michael Grünewald
Date: Thu Jul 10 11:02:19 CEST 2014

Gasoline (https://github.com/michipili/gasoline)
This file is part of Gasoline

Copyright © 2014 Michael Grünewald

This file must be used under the terms of the CeCILL-B.
This source file is licensed as described in the file COPYING, which
you should have received as part of this distribution. The terms
are also available at
http://www.cecill.info/licences/Licence_CeCILL-B_V1-en.txt *)
(** Dynamically typed values for plain applications. *)

(** The type tracking ['a]. *)
type 'a kind =
| Bool : bool kind
| Int : int kind
| Char : char kind
| String : string kind
| Float : float kind

(** The abstract type of dynalically typed values. *)
type t


val make : 'a kind -> 'a -> t
(** [make kind value] create a dynamically typed value. *)

val get_option : 'a kind -> t -> 'a option
(** Get a dynamical value in a safe manner. *)

val get : 'a kind -> t -> 'a
(** Get a dynamical value in an unsafe manner.

@raise Failure if the value is of anouther kind. *)

val has_kind : 'a kind -> t -> bool
(** Predicate recognising dynamical values of a specific kind. *)

val same_kind : t -> t -> bool
(** Predicate recognising dynamic of values of the same kind. *)


(** {6 Persistence} *)

val taste : 'a kind -> string -> bool
(** [taste k s] recognise [s] if it represents a value of kind [k]. *)

val to_string : t -> string
(** [to_string v] transform [v] to a string.

This string can be written in a file and read back later with [of_string]. *)

val of_string : string -> t
(** [of_string s] convert a string to a dynamic value. *)

val of_string_kind : 'a kind -> string -> 'a
(** [of_string_kind k s] convert [s] to a dynamic value of kind [k].

@raise Failure if [s] cannot be converted to a value of kind [k]. *)

val of_string_same : t -> string -> t
(** [of_string_same v s] convert [s] to a dynamic value of the same kind a [v].

@raise Failure if [s] cannot be converted to a value of that kind. *)

val reader_kind : 'a kind -> Scanf.Scanning.in_channel -> 'a
(** Custom reader to use with [scanf]. *)

val reader_same : t -> Scanf.Scanning.in_channel -> t
(** Custom reader to use with [scanf]. *)

val printer : Format.formatter -> t -> unit
(** A printer for the toplevel. *)
