(* Gasoline_Plain_Application -- C-Stylish Applications

Author: Michael Grünewald
Date: Sat Dec 28 13:05:16 CET 2013

Gasoline (https://github.com/michipili/gasoline)
This file is part of Gasoline

Copyright © 2013 Michael Grünewald

This file must be used under the terms of the CeCILL-B.
This source file is licensed as described in the file COPYING, which
you should have received as part of this distribution. The terms
are also available at
http://www.cecill.info/licences/Licence_CeCILL-B_V1-en.txt *)
(** Plain General Applications. *)

(** Application components. *)
module Component :
sig

  (** The abstract type of application components. *)
  type t

  (** Register a component. *)
  val make :
    ?bootstrap:(unit -> unit) ->
    ?shutdown:(unit -> unit) ->
    ?require:(string list) ->
    ?provide:(string list) ->
    ?version:string ->
    ?config_prefix: string list ->
    ?getopt_prefix: char ->
    name:string ->
    description:string ->
    unit -> t
end

(** Configuration values. *)
module Configuration :
sig

  (** The type tracking ['a]. *)
  type 'a kind =
  | Bool : bool kind
  | Int : int kind
  | Char : char kind
  | String : string kind
  | Float : float kind


  (** The type of application components. *)
  type component =
    Component.t

    val make : 'a kind -> component ->
      ?flag:char -> ?env:string -> ?shy:bool ->
      string -> 'a -> string -> (unit -> 'a)
    (** [make kind comp name default description] create a
        configuration item holding a value of type ['a] and return an
        accessor for that item.

        @param name the name of the parameter in configuration files.
        @param flag the letter used for command line flag.
        @param env the environment variable used to get a value.
        @param shy flag governing description in the short help. *)

    (** The type of configuration specifications. *)
    type spec =
    | Empty
    | Command_line
    | Environment
    | File of string
    | UserFile of string list * string
    | Heredoc of string
    | Alist of ((string list * string) * string) list
    | Merge of spec * spec
    | Override of spec * spec
end
with type 'a kind = 'a Gasoline_Plain_Definition.Value.kind

val run : string -> string -> string ->
  ?notes:(Getopts.note list) ->
  ?configuration:Configuration.spec ->
  (string list -> unit) -> unit
(** [run name usage description ?notes main] run the application.

    The function [main] is passed the list of remaining arguments on the
    command line. *)
