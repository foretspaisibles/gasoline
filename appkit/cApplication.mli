(* CApplication -- C-Stylish Applications

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
(** C-Stylish Applications. *)


(** Dynamically typed values. *)
module Value :
sig

  (** The type tracking ['a]. *)
  type 'a kind =
  | Bool : bool kind
  | Int : int kind
  | Char : char kind
  | String : string kind
  | Float : float kind

  type t
  val make : 'a kind -> 'a -> t

end


(** Diagnostic messages. *)
module Message :
sig

  (** The type of message classification. *)
  type classification =
  | Debug
  | Info
  | Notice
  | Warning
  | Error
  | Critical
  | Alert		(** A condition that should be corrected immediately. *)
  | Emergency		(** A panic condition. *)

  (** The abstract type of message sinks. *)
  type sink


  (** [send sink clas id binding] require the sink [sink] to send the
  message identified by [id] with classification [c], substituting
  message variables according to the [binding]. *)
  val send : sink -> classification ->
    string -> (string * Value.t) list -> unit


end


(** Application components. *)
module Component :
sig

  (** The concrete type of application components. *)
  type t = {
    name: string;
    version: string;
    require: string list;
    description: string;
    config_prefix: string list;
    getopt_prefix: char option;
  }

  val sink : t -> Message.sink
  (** Get the message sink dedicated to a given component. *)

end


(** Configuration values. *)
module Configuration :
sig

  type 'a t
  (** The type of configuration items tracking a value of type ['a]. *)

  val make : 'a Value.kind -> Component.t ->
    ?flag:char -> ?env:string -> ?shy:bool ->
    string -> 'a -> string -> 'a t
  (** [make kind comp name default description] create a configuration
      item holding a value of type ['a].

      @param flag the letter used for command line flag.
      @param env the environment variable used to get a value.
      @param shy flag governing description in the short help. *)

  val get : 'a t -> 'a
  (** [get item] return the value of the given configuration [item]. *)

  val set : 'a t -> 'a -> unit
  (** [set item value] set the [value] of the given configuration [item]. *)

end


(** Analyse of command line arguments. *)
module Getopt :
sig

  type t
  (** The abstract type of getopt thingies. *)

  val flag : char -> (unit -> unit) -> string -> t
  (** [flag c callback description] return a getopt thingie recognising
      the flag [c] on the command line and triggering the given
      [callback]. *)

  val make : 'a Value.kind -> char -> ('a -> unit) -> string -> t
  (** [make kind c callback description] return a getopt thingie recognising
      the flag [c] on the command line. When this flag occurs on the command
      line, the [callback] is called on the option argument. The message
      [description] may be displayed in the help screen of the program.

      If the flag is present without an argument or if the argument cannot be
      converted, then the guest program will terminate with exit code
      [EXIT_USAGE] (64), displaying appropriate message and a usage notice.

      The callback may raise [Invalid_argument] to indicate that a specific
      value of the option is not supported.  The guest program will terminate
      with exit code [EXIT_USAGE] (64), displaying appropriate message and
      a usage notice. *)

  type spec
  (** The abstract type of the whole analyse specification. *)

  val note : string -> string -> t
  (** [note title text] create a note to be displayed in the help summary
      of the application.  This can be used to add copyright information
      or any other kind of credit or information to the help summary. *)

  val spec : string -> string -> t list -> (string -> unit) -> spec
  (** [spec usage description opt rest] create a full command line analyse
      specification out of its components. *)

  val help : spec -> unit
  (** Output the help summary associated to the analysis specification. *)

  val help_message : spec -> string
  (** Return the help summary associated to the analysis specification. *)

end


val getopt_list : unit -> Getopt.t list

val getopt : Getopt.spec -> unit

val init : unit -> unit
