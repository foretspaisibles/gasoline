(* CApplication -- Application using the C locale

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

      @parameter flag the letter used for command line flag.
      @parameter env the environment variable used to get a value.
      @parameter shy flag governing description in the short help. *)

  val get : 'a t -> 'a
  (** [get item] return the value of the given configuration [item]. *)

  val set : 'a t -> 'a -> unit
  (** [set item value] set the [value] of the given configuration [item]. *)

end


module Getopt :
sig

  type t
  (** The abstract type of getopt thingies. *)

  val flag : char -> (unit -> unit) -> string -> t
  (** [flag c callback description] return a getopt thingie recogising
      the flag [c] on the command line and triggering the given
      [callback]. *)

  val make : 'a Value.kind -> char -> ('a -> unit) -> string -> t

  type spec

  val note : string -> string -> t

  val spec : string -> string -> t list -> (string -> unit) -> spec

  val help : spec -> unit

  val help_message : spec -> string

end


val getopt_list : unit -> Getopt.t list

val getopt : Getopt.spec -> unit

val init : unit -> unit
