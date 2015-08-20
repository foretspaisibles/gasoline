(* Gasoline_Generic_Component -- Application components

Author: Michael Grünewald
Date: Wed Sep 17 10:16:30 CEST 2014

Gasoline (https://github.com/michipili/gasoline)
This file is part of Gasoline

Copyright © 2014 Michael Grünewald

This file must be used under the terms of the CeCILL-B.
This source file is licensed as described in the file COPYING, which
you should have received as part of this distribution. The terms
are also available at
http://www.cecill.info/licences/Licence_CeCILL-B_V1-en.txt *)
(** Generic application components. *)

(** The type of software component information cards. *)
type info = {

  (** Name of the software component. *)
  name : string;

  (** Version information. *)
  version : string;

  (** One-line description. *)
  description : string;

  (** List of required components. *)
  require: string list;

  (** List of provided conditions. *)
  provide : string list;

  (** Configuration path of the component *)
  config_prefix : string list;

  (** Character introducing long options used by [Getopt]. *)
  getopt_prefix : char option;
}

(** The type of component callbacks. *)
type callback = info -> unit

(** The abstract type of components. *)
type t

(** [make info boostrap shutdown] prepares a software component
identified by [info] and using the callbacks [boostrap] and
[shutdown]. *)
val make : info -> callback -> callback -> t

val bootstrap : t list -> unit
(** Sorts the given component list according to the [require] and
[provide] fields and bootstrap these in order.

@raise Failwith if the specification cannot be satisfied. *)

val shutdown : t list -> unit
(** Sorts the given component list according to the [require] and
[provide] fields and shutdown these in order.

@raise Failwith if the specification cannot be satisfied. *)
