(* Automaton -- Simulate dictionaries

Author: Michael Grünewald
Date: Thu Nov  7 23:02:35 CET 2013

Gasoline (https://github.com/michipili/gasoline)
This file is part of Gasoline

Copyright © 2013 Michael Grünewald

This file must be used under the terms of the CeCILL-B.
This source file is licensed as described in the file COPYING, which
you should have received as part of this distribution. The terms
are also available at
http://www.cecill.info/licences/Licence_CeCILL-B_V1-en.txt *)

(** Simulate dictionaries. *)


(** The input signature for the [Make] functor. *)
module type P =
sig

  val length : int
  (** The length of a state element. *)

  val generate_min : int
  (** The minimal size of a generated element. *)

  val generate_max : int
  (** The maximal size of a generated element. *)

end

(** The output signature of the [Make] functor. *)
module type S =
sig

  type t
  (** The abstract type of word generator automaton. *)

  type table
  (** The abstract type outcome tables. *)

  val create : table -> t
  (** Create a word generator automaton from transition tables. *)

  val generate : t -> string
  (** Generate a word. *)

end

module Make(P:P): S
  with type table = Table.Make(P).t
