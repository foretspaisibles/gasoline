(* Generic_message -- A generic message facility

Author: Michael Grünewald
Date: Thu Aug 23 07:15:48 CEST 2012

Copyright © 2012 Michael Grünewald

This file must be used under the terms of the CeCILL-B.
This source file is licensed as described in the file COPYING, which
you should have received as part of this distribution. The terms
are also available at
http://www.cecill.info/licences/Licence_CeCILL-B_V1-en.txt *)
(** Generic message facility. *)

open Generic_type

(** Description of error conditions used by Scribe and Sender. *)
type error =
| Invalid_message of string
| Message_not_found of string
| Variable_not_found of string

(** The exception thrown by Scribe and Sender functions. *)
exception Error of error

(** Lexing message strings containing variables *)
module Lexer :
sig

  (** A message string containing variables is encoded as a string.
  Within this string, a sequence [${identifier}] indicates a variable,
  the identifier can be any non empty sequence of characters from the
  set [[A-Za-z0-9_]]. A sequence [${identifier:format}] indicates a
  variable with formatting information (pretty much like
  printf). Formatting information can be any sequence of characters
  from the set [[A-Za-z0-9_+-.,]] but this string must also be
  validated by the function [V.format_of_string].

  An escaped $ character is a $ that immediately follows a backslash
  character; it then stands for a plain $.  *)

  (** Position in an input stream. *)
  type pos = {
    mutable char : char;
    mutable char_no : int;
    mutable line_no : int;
    mutable offset: int;
  }

  (** The type of lexing errors. *)
  type error =
  | Invalid_char_in_identifier of char
  | Invalid_char_in_format of char
  | Unterminated_substitution

  (** The exception thrown by lexing functions. *)
  exception Error of pos * error

  (** The abstract type of lexers. *)
  type t

  (** Create a fresh new lexer. *)
  val create : unit -> t

  (** The type of lexemes. *)
  type lexeme =
  | Text of string
  | Variable of string
  | Formatted_variable of string * string
  (* The order of args is value, format *)

  (** Analysing a character stream.

  @raise Error when the character stream is not well-formed. *)
  val analyse : t -> char Stream.t -> lexeme list

  (** Analysing a character string.

  @raise Error when the string is not well-formed. *)
  val analyse_string : t -> string -> lexeme list

end

(** The input signature for the functors [Scribe.Make], [Sender.Make]
and [Sink.Make]. *)
module type P =
sig

  module Data : DATA

  module Locale : LOCALE

  module Buffer : BUFFER
    with type word = Data.word
    and type block = Data.block
    and type out_channel = Data.out_channel

  module Value : VALUE
    with type buffer = Buffer.t
    and type locale = Locale.t

  module Database : DATABASE

  module Classification : CLASSIFICATION
    with type locale = Value.locale
    and type buffer = Buffer.t
    and type out_channel = Data.out_channel

end

(** Message scribe. *)
module Scribe :
sig

  (** The output signature of the functor [Scribe.Make]. *)
  module type S =
  sig

    (** The type of message scribes. *)
    type t

    (** The type of message classifications. *)
    type classification

    (** The type of message database connection tokens. *)
    type connection_token

    (** The type of output channels. *)
    type out_channel

    (** The type of variable values that can occur in a message. *)
    type value

    (** The type of locales. *)
    type locale

    (** The type of data buffers. *)
    type buffer

    (** The type of data blocks. *)
    type block

    (** Make a message scribe.

    The message scribe holds a handler to the database open thanks to
    the [connection_token]. *)
    val make : connection_token -> locale -> t

    (** [add scribe c id bindings] adds the characters of the message
    [id] classified as [c] and with bindings [bindings] to an internal
    buffer in [scribe]. *)
    val add : t -> classification -> string -> (string * value) list -> unit

    (** Empty internal buffer. *)
    val clear : t -> unit

    (** Empty and reset internal buffer. *)
    val reset : t -> unit

    (** Return the contents of the internal buffer.  The internal
    buffer itself is not changed. *)
    val contents : t -> block

    (** Output the contents of the internal buffer.  The internal
    buffer itself is not changed. *)
    val output : out_channel -> t -> unit

    (** [transfer b scribe] copy each word in the internal storage of
    [scribe] to [b].  The internal buffer of [scribe] is then
    emptied. *)
    val transfer : buffer -> t -> unit

  end

  module Make(P:P): S
    with type classification = P.Classification.t
     and type locale = P.Locale.t
     and type connection_token = P.Database.connection_token
     and type out_channel = P.Data.out_channel
     and type value = P.Value.t
     and type locale = P.Locale.t
     and type buffer = P.Buffer.t
     and type block = P.Data.block

end

(** Message sender. *)
module Sender :
sig

  (** The output signature of the functor [Sender.Make]. *)
  module type S =
  sig

    (** The type of message senders. *)
    type t

    (** The type of message sender control bits.

    A message sender operating with the [async] bit set never flushes
    implicitly its backend, otherwise it flushes its backend after each
    emission.

    A message sender operating with the [delay] bit set does not emit the
    messages it receives but gather them in an internal storage. These
    messages can then be emitted or discarded, see [proceed] and
    [discard] below.

    A message sender operating with the [drop] bit set drops every
    messagess it becomes, without actually doing anything.

    A fresh new message sender operates with all its control bits set to
    false. *)
    type control = {
      mutable async: bool;	(** Buffer messages *)
      mutable delay: bool;	(** Delay messages *)
      mutable drop: bool;       (** Drop messages *)
    }

    (** Return the control bits of a message sender.

    The bits are mutable and can then be edited. *)
    val get_control : t -> control

    (** The type of message classifications. *)
    type classification

    (** The type of message database connection tokens. *)
    type connection_token

    (** The type of output channels. *)
    type out_channel

    (** The type of variable values that can occur in a message. *)
    type value

    (** The type of locales. *)
    type locale

    (** Make a message sender. *)
    val make : connection_token -> locale -> out_channel -> t

    (** [send s c id binding] requires the sender [s] to send the
    message identified by [id] with classification [c], substituting
    message variables according to the [binding].

    @raise Variable_not_found to indicate a variable occuring
      in the message that is not bound in the [binding].
    @raise Message_not_found to indicate the concrete form
      corresponding to [id] on the message.
    @raise Invalid_message to indicate an invalid concrete form
      for the message. The concrete form is given as argument to the
      error constructor and may be analysed with the lexer to find the
      precise origin of the error.  *)
    val send : t -> classification -> string -> (string * value) list -> unit

    (** Require a sender to process all pending messages and to flush
    its backend. *)
    val sync : t -> unit

    (** Another name for sync. *)
    val flush : t -> unit

    (** Another name for sync. *)
    val proceed : t -> unit

    (** Require a sender to discard all pending messages. *)
    val discard : t -> unit

  end

  module Make(P:P): S
    with type classification = P.Classification.t
     and type locale = P.Locale.t
     and type connection_token = P.Database.connection_token
     and type out_channel = P.Data.out_channel
     and type value = P.Value.t

end


(** Message sinks.

This interface is very similar to [Sender] but distinct and
incompatible: the way values are created is different.


The main differences are the ability to defer connection to a message
databank, and to attach several output channels to a message sink.

It is thus possible to use message sinks at the initialisation time of
a program, delaying the databank connection to a later time. *)
module Sink :
sig

  (** The output signature of the functor [Sink.Make]. *)
  module type S =
  sig

    (** The type of message sinks. *)
    type t

    (** Create a message sink.

    To become fully operational, a message sink must be connected to a
    (locale,databank) pair with [connect] below, and attached to one
    or more output channel with [attach] below.

    A sink that is not connected to a locale and a databank behaves as
    if the control bit delay were set.

    A sink that is conencted to a locale and a databank but is not
    attached to any output channel nehaves as if the drop bit were
    set. *)
    val create : unit -> t

    (** The type of message classifications. *)
    type classification

    (** The type of message database connection tokens. *)
    type connection_token

    (** The type of output channels. *)
    type out_channel

    (** The type of variable values that can occur in a message. *)
    type value

    (** The type of locales. *)
    type locale
    (** The type of message sink control bits.

    A message sink operating with the [async] bit set never flushes
    implicitly its backend, otherwise it flushes its backend after each
    emission.

    A message sink operating with the [delay] bit set does not emit the
    messages it receives but gather them in an internal storage. These
    messages can later be emitted or discarded, see [proceed] and
    [discard] below.

    A message sink operating with the [drop] bit set drops every
    messagess it becomes, without actually doing anything.

    Capping (resp. flooring) messages: if a cap (resp. floor) is
    given, messages will reach the sink, only if they have a
    classification at most (resp. at least) as important as the given
    one.

    A fresh new message sink operates with all its control bits set to
    false, cap and floor set to [None].
    *)
    type control = {
      mutable async: bool;	                (** Buffer messages *)
      mutable delay: bool;	                (** Delay messages *)
      mutable drop: bool;                 	(** Drop messages *)
      mutable cap: classification option; 	(** Cap messages *)
      mutable floor: classification option; 	(** Floor messages *)
    }

    (** Return the control bits of a message sink.

    The bits are mutable and can then be edited. *)
    val get_control : t -> control

    (** Connect a message sink with a databank. A previous
    connection, if any, is closed.

    After connection, pending messages are proceeded, as if the
    [proceed] procedure below had been called. *)
    val connect : t -> connection_token -> locale -> unit

    (** Disconnect the message sink from its databank. *)
    val disconnect : t -> unit

    (** Predicate recognising connected sinks. *)
    val is_connected : t -> bool

    (** Attach an output channel to a sink. *)
    val attach : t -> out_channel -> unit

    (** Detach all output channels from a sink. *)
    val detach : t -> unit

    (** Iter over output channels attached to a sink.

    It is an error to close output channels attached to a sink. *)
    val iter : (out_channel -> unit) -> t -> unit

    (** The list of output channels attached to our sink. *)
    val out_channels : t-> out_channel list

    (** [send s c id binding] requires the sink [s] to send the
    message identified by [id] with classification [c], substituting
    message variables according to the [binding].

    @raise Variable_not_found to indicate a variable occuring
      in the message that is not bound in the [binding].
    @raise Message_not_found to indicate the concrete form
      corresponding to [id] on the message.
    @raise Invalid_message to indicate an invalid concrete form
      for the message. The concrete form is given as argument to the
      error constructor and may be analysed with the lexer to find the
      precise origin of the error.  *)
    val send : t -> classification -> string -> (string * value) list -> unit

    (** Require a sink to process all pending messages and to flush
    its backend.

    On a non connected sink, this is a no-op. *)
    val sync : t -> unit

    (** Another name for sync. *)
    val flush : t -> unit

    (** Another name for sync. *)
    val proceed : t -> unit

    (** Require a sink to discard all pending messages. *)
    val discard : t -> unit

  end

  module Make(P:P): S
    with type classification = P.Classification.t
    and type locale = P.Locale.t
    and type connection_token = P.Database.connection_token
    and type out_channel = P.Data.out_channel
    and type value = P.Value.t
end
