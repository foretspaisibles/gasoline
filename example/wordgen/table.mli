(* Table -- Automaton table

Author: Michael Grünewald
Date: Fri Nov  8 07:55:07 2013 CET

Gasoline (https://github.com/michipili/gasoline)
This file is part of Gasoline

Copyright © 2013 Michael Grünewald

This file must be used under the terms of the CeCILL-B.
This source file is licensed as described in the file COPYING, which
you should have received as part of this distribution. The terms
are also available at
http://www.cecill.info/licences/Licence_CeCILL-B_V1-en.txt *)

(** Automaton table. *)

(** Output signature for the [Make] functor. *)
module type S =
sig

  (** {6 Transition tables} *)

  type t
  (** The abstract type of transition tables. *)

  val empty : t
  (** The empty transition table. *)

  val add_word : t -> string -> t
  (** Add a word to a transition table. *)

  val add_list : t -> string list -> t
  (** Add the words of the given list to a transition table. *)

  val add_stream : t -> string Stream.t -> t
  (** Add the words of the given stream to a transition table. *)

  val add_file : t-> string -> t
  (** Add the words of the given file (one on each line)
  to a transition table.

  @raise Sys_error if the file cannot be opened. *)


  (** {6 Transition table states} *)

  type state
  (** The abstract type of initial states. *)

  val initial : state
  (** The initial state. *)

  val outcome : t -> state -> char * state
  (** Choose an outcome based on the given table and state. *)


  (** {6 Printing} *)

  val format : Format.formatter -> t -> unit
  (** Format a table on a given pretty-printer. *)

  (** {6 Persistance} *)

  val to_persistant : t -> Persistant.t
  (** Transform a table into persitant data. *)

  val of_persistant : Persistant.t -> t
  (** The inverse of [to_persistant].
  @raise Invalid_arg if the persistant value is bad. *)

end


(** Input signature of the [Make] functor. *)
module type P =
sig

  val length : int
  (** The length of a state element. *)

end


(** Functor building an implementaion of the table structure given a length. *)
module Make(P:P): S
