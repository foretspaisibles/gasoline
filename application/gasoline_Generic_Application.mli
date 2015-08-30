(* Gasoline_Generic_Application -- Generic application

Author: Michael Grünewald
Date: Sun May 12 13:22:40 CEST 2013

Copyright © 2013 Michael Grünewald

This file must be used under the terms of the CeCILL-B.
This source file is licensed as described in the file COPYING, which
you should have received as part of this distribution. The terms
are also available at
http://www.cecill.info/licences/Licence_CeCILL-B_V1-en.txt *)
(** Generic application. *)

(** The input signature for the functor [Generic_application.Make].

    This signature describes an implementation of dynamically typed values. *)
module type P =
sig
end


(** The output signature of the functor [Make]. *)
module type S =
sig

  (** Application components. *)
  module Component :
  sig

    (** The abstract type of application components. *)
    type t

    (** Create a component

        The callbacks [bootstrap] and [shutdown] are triggered when
        initialising the program. The [require] and [provide] fields
        can be used to ordre initialisation of software components. *)
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
    (** [make value_of_string comp name default description] create a
        configuration item holding a value of type ['a] and return an
        accessor for that item.

        @param name the name of the parameter in configuration files.
        @param flag the letter used for command line flag.
        @param env the environment variable used to get a value.
        @param shy flag governing description in the short help.
        @param optarg is the value of an option argument

        If the value optarg is set, then the generated command-line
        flag has nor argument and uses the value of [optarg] as an
        argument.

        The [value_of_string] function should raise a [Failure]
        exception when it cannot converts a string. *)

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
end

(** Functor building an implementation of a generic application. *)
module Make(Parameter:P): S
