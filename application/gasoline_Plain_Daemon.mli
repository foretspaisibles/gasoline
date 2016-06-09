(* Gasoline_Plain_Daemon -- C-Stylish Unix Daemons

Gasoline (https://github.com/michipili/gasoline)
This file is part of Gasoline

Copyright © 2013–2015 Michael Grünewald

This file must be used under the terms of the CeCILL-B.
This source file is licensed as described in the file COPYING, which
you should have received as part of this distribution. The terms
are also available at
http://www.cecill.info/licences/Licence_CeCILL-B_V1-en.txt *)
(** Plain Daemon Applications. *)

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

  (** Set the callbacks [boostrap] and [shutodown] of a component. *)
  val set_callbacks :
    ?bootstrap:(unit -> unit) ->
    ?shutdown:(unit -> unit) ->
    t -> unit
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
    (** A version of [make] specialised on the identity function. *)
end

val run : string -> string -> string ->
  ?notes:((string * string) list) ->
  ?system_configuration:string ->
  ?chdir:string ->
  ?stdout:string ->
  ?stderr:string ->
  (string list -> unit) -> unit
(** [run name usage description main] run the daemon.

    If the flag "-F" is given, then the daemon stays in the
    foreground.  If [stdout] and or [stderr] are given and start with
    a character ['/'] then they are interpreted as files where stdin
    and stderr are redirected to.  Otherwise, they are intpreted as
    subprocesses where these descriptors should be redirected to. *)
