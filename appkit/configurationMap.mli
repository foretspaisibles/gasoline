(* ConfigurationMap -- Generic configuration facility

Author: Michael Grünewald
Date: Wed Oct 24 07:48:50 CEST 2012

Copyright © 2012 Michael Grünewald

This file must be used under the terms of the CeCILL-B.
This source file is licensed as described in the file COPYING, which
you should have received as part of this distribution. The terms
are also available at
http://www.cecill.info/licences/Licence_CeCILL-B_V1-en.txt *)
(** Configuration maps.

A configuration map holds a dictionary mapping configuration keys to
configuration values.  Values available in a configuration map can
retrieved based on a {i key} or consumed by a {i callback}.
Configuration maps can be combined together. *)

(** The abstract type of configuration maps. *)
type t


(** {6 Keys and value retrieval} *)

(** The type of configuration values concrete representations.
A concrete representation should use the [parse_error] function below
to advertise errors. *)
type 'a concrete = {
  of_string: string -> 'a;
  to_string: 'a -> string;
}

(** The type of configuration keys.  A configuration key can be used
to retrieve a configuration value. *)
type 'a key = {
  concrete: 'a concrete;
  path: string list;
  name: string;
  default: 'a;
  description: string;
}

(** The abstract type of configuration callbacks. *)
type callback

(** [callback key callback] create a configuration callback consuming keys
described by [key] with the given [callback]. *)
val callback : 'a key -> ('a -> unit) -> callback

(** [key concrete path name default description] create a key
out of its given parts. *)
val key : ('a concrete) -> string list -> string -> 'a -> string -> 'a key

(** Get the value associated with an key.  On error conditions, the
default value from the key is returned. *)
val get : t -> 'a key -> 'a

(** Explicitely consume the given value with the provided callback. *)
val apply : t -> callback -> unit

(** [value key text] get the value associated to [text] as if it
were assigned to [key]. *)
val value : 'a key -> string -> 'a


(** {6 Operations on configuration maps} *)

(** The empty configuration. *)
val empty : t

(** Add a configuration binding. *)
val add : t -> (string list * string) -> string -> t

(** [merge a b] a configuration object looking up values in [a] then
in [b]. *)
val merge : t -> t -> t

(** [override a b] a configuration object whose keys are the same as
[a] and the values are possibly overriden by those found in [b]. *)
val override : t -> t -> t

(** Read configuration values from a file. *)
val from_file : string -> t

(** Read configuration values from a string. *)
val from_string : string -> t

(** Read configuration values from an alist. *)
val from_alist : ((string list * string) * string) list -> t
