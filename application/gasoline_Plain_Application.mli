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

  (** The type of application components. *)
  type component =
    Component.t

  val make : (string -> 'a) -> component ->
      ?optarg:string ->
      ?flag:char -> ?env:string -> ?shy:bool ->
      string -> 'a -> string -> (unit -> 'a)
    (** [make kind comp name default description] create a
        configuration item holding a value of type ['a] and return an
        accessor for that item.

        @param name the name of the parameter in configuration files.
        @param flag the letter used for command line flag.
        @param env the environment variable used to get a value.
        @param shy flag governing description in the short help. *)


    val make_bool : component ->
      ?optarg:string -> ?flag:char -> ?env:string -> ?shy:bool ->
      string -> bool -> string -> (unit -> bool)
    (** A version of [make] specialised on [bool_of_string]. *)

    val make_int : component ->
      ?optarg:string -> ?flag:char -> ?env:string -> ?shy:bool ->
      string -> int -> string -> (unit -> int)
    (** A version of [make] specialised on [int_of_string]. *)

    val make_float : component ->
      ?optarg:string -> ?flag:char -> ?env:string -> ?shy:bool ->
      string -> float -> string -> (unit -> float)
    (** A version of [make] specialised on [float_of_string]. *)

    val make_string : component ->
      ?optarg:string -> ?flag:char -> ?env:string -> ?shy:bool ->
      string -> string -> string -> (unit -> string)
    (** A version of [make] specialised on the identitiy function. *)

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

val run : string -> string -> string ->
  ?notes:((string * string) list) ->
  ?configuration:Configuration.spec ->
  (string list -> unit) -> unit
(** [run name usage description ?notes main] run the application.

    The function [main] is passed the list of remaining arguments on the
    command line. *)
