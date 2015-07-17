(* Gasoline_Generic_Asset -- Generic assets

Author: Michael Grünewald
Date: Wed Sep  3 12:55:04 CEST 2014

Gasoline (https://github.com/michipili/gasoline)
This file is part of Gasoline

Copyright © 2014 Michael Grünewald

This file must be used under the terms of the CeCILL-B.
This source file is licensed as described in the file COPYING, which
you should have received as part of this distribution. The terms
are also available at
http://www.cecill.info/licences/Licence_CeCILL-B_V1-en.txt *)
(** Generic assets. *)

(** Setup asset information. *)
val setup :
  ('a -> 'b) ->
  package:string (** Name of the application package *) ->
  version:string (** Version information *) ->
  ?prefix:string (** Installation prefix *) ->
  ?bindir:string (** User programs directory *) ->
  ?sbindir:string (** System administrator programs directory *) ->
  ?libexecdir:string (** Ancillary programs directory *) ->
  ?datadir:string (** Read-only architecture-independant data files *) ->
  ?sysconfdir:string (** Host configuration, read-only *) ->
  ?usrconfdir:string (** User configuration, read-only *) ->
  ?sharedstatedir:string (** Shared state directory, e.g. /com *) ->
  ?localstatedir:string (** Local state directory, e.g. /var *) ->
  'a -> 'b

(** Create a path in the user programs directory. *)
val bin : string -> string

(** Create a path in the system administrator programs directory. *)
val sbin : string -> string

(** Create a path in the anclillary programs directory. *)
val libexec : string -> string

(** Create a path in the data directory for read-only,
architecture-independant files. *)
val data : string -> string

(** Create a path in the host-configuration directory. *)
val sysconf : string -> string

(** Create a path in the user-configuration directory. *)
val usrconf : string -> string

(** Create a path in the shared state directory. *)
val sharedstate : string -> string

(** Create a path in the local state directory. *)
val localstate : string -> string
