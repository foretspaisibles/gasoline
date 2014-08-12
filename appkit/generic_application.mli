(* Generic_application -- Generic application

Author: Michael Grünewald
Date: Sun May 12 13:22:40 CEST 2013

Copyright © 2013 Michael Grünewald

This file must be used under the terms of the CeCILL-B.
This source file is licensed as described in the file COPYING, which
you should have received as part of this distribution. The terms
are also available at
http://www.cecill.info/licences/Licence_CeCILL-B_V1-en.txt *)
(** Software components. *)

(** The type of software component registry card. *)
type component = {
  (** Name of the software component. *)
  name : string;

  (** Version information. *)
  version : string;

  (** List of required components. *)
  require: string list;

  (** One-line description of the component. *)
  description: string;

  (** Configuration path of the component *)
  config_prefix : string list;

  (** Character introducing long options used by [Getopt]. *)
  getopt_prefix : char;
}

(** The type of callbacks. *)
type callback = unit -> unit

(** The type of concrete configuration values. *)
type 'a concrete =
  'a Configuration.concrete

(** The type of configuration specifications. *)
type configuration_spec =
| Empty
| Command_line
| Environment
| File of string
| Heredoc of string
| Alist of ((string list * string) * string) list
| Merge of configuration_spec * configuration_spec
| Override of configuration_spec * configuration_spec

(** The output signature of the functor [Make]. *)
module type S =
sig

  (** Identify the application. *)
  val identify :
    name:string (** Name of the application *) ->
    version:string (** Version information *) ->
    usage:string (** Usage on the command line *) ->
    description:string (** One-line description of the application. *) ->
    copyright:string (** Copyright notice *) ->
    license:string (** License notice *) ->
    author:string list (** Author *) ->
    ?prefix:string (** Installation prefix *) ->
    ?bindir:string (** User programs directory *) ->
    ?sbindir:string (** System administrator programs directory *) ->
    ?libexecdir:string (** Ancillary programs directory *) ->
    ?datadir:string (** Read-only architecture-independant data files *) ->
    ?sysconfdir:string (** Host configuration, read-only *) ->
    ?usrconfdir:string (** User configuration, read-only *) ->
    ?sharedstatedir:string (** Shared state directory, e.g. /com *) ->
    ?localstatedir:string (** Local state directory, e.g. /var *) ->
    unit -> unit

  (** {2 File paths} *)

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


  (* {2 Software components} *)

  (** Register a component. *)
  val register : ?bootstrap:callback -> ?shutdown:callback -> component -> unit

  (** Iter over components. *)
  val iter : (component -> unit) -> unit

  (** Fold over components. *)
  val fold : (component -> 'a -> 'a) -> 'a -> 'a

  (** The abstract type of message sinks. *)
  type sink

  (** Create or get the sink dedicated to a given component. *)
  val sink : component -> sink

  (** [config concrete component name default description] create a
  configuration option. *)
  val config : 'a concrete -> component -> ?flag:char -> ?env:string -> ?shy:bool -> string -> 'a -> string -> 'a ref

  (** {2 Command line} *)

  (** Register a command line flag. *)
  val flag : Getopt.t -> unit

  (** Register a note for the help message. *)
  val note : string -> string -> unit

  (** [main initsink config rest f] run the application. The [config]
  argument is the list of configuration files to read.  The [rest]
  argument is the rest function, processing non-option arguments on
  the command line. *)
  val main : configuration_spec -> (string -> unit) -> (unit -> unit) -> unit
end


module type SINK =
  Generic_message.Sink.S

(** The input signature for the functor [Generic_application.Make]. *)
module type P =
sig

  type sink
  (** The type of message sinks. *)

  val register : (Configuration.handler -> unit) -> ((unit -> unit) -> unit) -> component -> sink -> unit
  (** [register rkey rcb comp sink] is expected to create several
  configuration keys and callbacks and to register them with [rkey]
  and [rcb]. *)
end


module Make(Sink:SINK)(Parameter:P with type sink = Sink.t): S
  with type sink = Sink.t
