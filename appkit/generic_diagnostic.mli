(* Generic_diagnostic -- Generic diagnostic facility

Author: Michael Grünewald
Date: Mon Oct 15 21:15:26 CEST 2012

Copyright © 2012 Michael Grünewald

This file must be used under the terms of the CeCILL-B.
This source file is licensed as described in the file COPYING, which
you should have received as part of this distribution. The terms
are also available at
http://www.cecill.info/licences/Licence_CeCILL-B_V1-en.txt *)
(** Generic message log facilities.

This generic diagnostic facilitiy imitates the classical Unix syslog
service, at the application level.  It uses the same basic
abstractions as [Generic_message] and allows to:
- issue messages to potentially various destinations, according to
  severity and facility.
- use multiple message databanks. *)

open Generic_type


(** Identification of software facilities.

Values identifying software facilities should be suitable for the
generic equal and hash functions.  Candidate types like integers and
strings are suitable. *)
module type FACILITY =
sig
  type t
end


(** The input signature for the functor [Make]. *)
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

  module Facility : FACILITY

end


(** The output signature for the functor [Make]. *)
module type S =
sig

  (** The abstract type of diagnostic loggers. *)
  type t

  (** Create a fresh new diganostic logger. *)
  val create : unit -> t

  (** The type of message sinks. *)
  type sink

  (** The type of application facilities. *)
  type facility

  (** Add a sink to the diagnostic logger. *)
  val add : t -> facility -> sink -> unit

  (** Remove a sink from the diagnostic logger.  If the given
  [facility] ist not bound to a sink, this is a no-op. *)
  val remove : t -> facility -> unit

  (** Check for a sink with the given facility. *)
  val mem : t -> facility -> bool

  (** Iterate over facilities and sinks of a diagnostic logger. *)
  val iter : (facility -> sink -> unit) -> t -> unit

  (** Fold over facilities and sinks of a diagnostic logger. *)
  val fold : (facility -> sink -> 'a -> 'a) -> t -> 'a -> 'a

  (** The type of message classifications. *)
  type classification

  (** The type of values that can be conveyed by messages. *)
  type value

  (** Send a message to a diagnostic logger. *)
  val send : t -> facility -> classification -> string -> (string * value) list -> unit

end


(** Functor building an implementation of the [Generic_diagnostic]
structure. *)
module Make(P:P): S
  with type sink = Generic_message.Sink.Make(P).t
   and type facility = P.Facility.t
   and type classification = P.Classification.t
   and type value = P.Value.t
   and type facility = P.Facility.t
