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
  (** The type tracking ['a]. *)
  type 'a kind

  (** The abstract type of dynamically typed values. *)
  type t

  val make : 'a kind -> 'a -> t
  (** [make kind value] create a dynamically typed value. *)

  val to_string : t -> string
  (** [to_string v] transform [v] to a string.

      This string can be written in a file and read back later with
      [of_string]. *)

  val of_string : string -> t
  (** [of_string s] convert a string to a dynamic value. *)

  val of_string_kind : 'a kind -> string -> 'a
  (** [of_string_kind k s] convert [s] to a dynamic value of kind [k].

      @raise Failure if [s] cannot be converted to a value of kind [k]. *)

  val kind_name : 'a kind -> string
  (** The name of a value kind. *)
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

    (** The type tracking ['a]. *)
    type 'a kind

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

  val run : string -> string -> string ->
    ?notes:(Getopts.note list) ->
    ?configuration:Configuration.spec ->
    (string list -> unit) -> unit
  (** [run name usage description ?notes main] run the application.

  The function [main] is passed the list of remaining arguments on the
  command line. *)
end

(** Functor building an implementation of a generic application. *)
module Make(Parameter:P): S
  with type 'a Configuration.kind = 'a Parameter.kind
