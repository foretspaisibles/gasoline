(* Generic_application -- Generic application

Author: Michael Grünewald
Date: Sun May 12 13:22:40 CEST 2013

Copyright © 2013 Michael Grünewald

This file must be used under the terms of the CeCILL-B.
This source file is licensed as described in the file COPYING, which
you should have received as part of this distribution. The terms
are also available at
http://www.cecill.info/licences/Licence_CeCILL-B_V1-en.txt *)
(** Generic application. *)

(** The local name of application component info cards. *)
type info =
  Generic_component.info

(** The local name of application component callbacks. *)
type callback =
  Generic_component.callback

(** The local name of application components. *)
type component =
  Generic_component.t


(** The local name of message sinks. *)
module type SINK =
  Generic_message.Sink.S

(** The module type of sink intitializers. *)
module type SINK_INITIALIZER =
sig
  type sink
  type connection_token
  type locale
  type out_channel

  val connection_token : info -> connection_token
  val locale : info -> locale
  val out_channel_lst : info -> out_channel list
end

(** The input signature for the functor [Generic_application.Make]. *)
module type P =
sig
  (** Dynamically typed values. *)
  module Value :
  sig

    (** The type tracking ['a]. *)
    type 'a kind

    (** The abstract type of dynamically typed values. *)
    type t
  end
end

(** The output signature of the functor [Make]. *)
module type S =
sig
  (** Adminisration of application components. *)
  module Component :
  sig

    (** The concrete type of application components. *)
    type info = {
      name : string;
      version : string;
      description : string;
      require: string list;
      provide : string list;
      config_prefix : string list;
      getopt_prefix : char option;
    }

    (** The type of callbacks. *)
    type callback = info -> unit

    (** The abstract type of message sinks. *)
    type sink

    (** Register a component. *)
    val register : ?bootstrap:callback -> ?shutdown:callback -> info -> unit

    (** Get the message sink dedicated to a given component. *)
    val sink : info -> sink
  end


  (** Configuration values. *)
  module Configuration :
  sig

    (** The type tracking ['a]. *)
    type 'a kind

    (** The type of configuration items tracking a value of type ['a]. *)
    type 'a t

    (** The type of application components. *)
    type component =
      Component.info

    val make : 'a kind -> component ->
      ?flag:char -> ?env:string -> ?shy:bool ->
      string -> 'a -> string -> 'a t
    (** [make kind comp name default description] create a configuration
        item holding a value of type ['a].  This configuration value can
        be manipulated through the functions [set] and [get] below.

        @param flag the letter used for command line flag.
        @param env the environment variable used to get a value.
        @param shy flag governing description in the short help. *)

    val get : 'a t -> 'a
    (** [get item] return the value of the given configuration [item]. *)

    val set : 'a t -> 'a -> unit
    (** [set item value] set the [value] of the given configuration [item]. *)
  end

  val run : string -> string -> string ->
    ?options:(Getopt.t list) -> ?notes:(Getopt.note list) ->
    (string list -> unit) -> unit
  (** [run name usage description ?options ?notes main] run the application.

  The function [main] is passed the list of remaining arguments on the
  command line. *)

  val help : unit -> unit
  (** Output the help summary associated to the application and
  terminates the application with exit code [SysExits.EXIT_SUCCESS].

  This is a magic function which can be called from code executed in the
  [run] function, if this condition is not fullfilled, it raises a
  [Invalid_argument].

  @raise Invalid_argument if not called under [run]. *)

  val usage : unit -> unit
  (** Output the usage summary associated to the application and
  terminate the application with exit code [SysExits.EXIT_USAGE].

  This is a magic function which can be called from code executed in the
  [run] function, if this condition is not fullfilled, it raises a
  [Invalid_argument].

  @raise Invalid_argument if not called under [run]. *)
end

module Make(Sink:SINK)
  (SinkInitializer:SINK_INITIALIZER
   with type sink = Sink.t
   and type connection_token = Sink.connection_token
   and type locale = Sink.locale
   and type out_channel = Sink.out_channel)(Parameter:P): S
  with type Component.sink = Sink.t
  and type 'a Configuration.kind = 'a Parameter.Value.kind
