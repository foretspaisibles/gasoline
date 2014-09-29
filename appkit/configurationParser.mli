(* ConfigurationParser -- Parsing configuration files

Author: Michael Grünewald
Date: Mon Oct 29 07:36:58 CET 2012

Copyright © 2012 Michael Grünewald

This file must be used under the terms of the CeCILL-B.
This source file is licensed as described in the file COPYING, which
you should have received as part of this distribution. The terms
are also available at
http://www.cecill.info/licences/Licence_CeCILL-B_V1-en.txt *)
(** Parsing configuration files.

Configuration files in the form of INI files that were common under
Microsoft Windows have grown popular in the open source world, and are
used my some major pieces of software, like Subversion or GIT.

We provide a parser to analyse these files.  This parser is a functor
parameterised by handlers for the events found in a configuration
file.  These events are of the following kinds:
- comments;
- sections;
- bindings;
- parse errors. *)


(** Position in an input stream. *)
type pos = Lexing.position

(** The type of excerpts. *)
type excerpt

(** The start position of an excerpt. *)
val startpos : excerpt -> pos

(** The end position of an excerpt. *)
val endpos : excerpt -> pos

(** The text of an excerpt. *)
val text : excerpt -> string

(** Parsing errors. *)
type error =
| Illegal_character
| Illegal_escape_sequence
| Unterminated_string
| Expecting_binding
| Expecting_value
| Expecting_path_elt
| Expecting_path_sep_or_term

val error_to_string : error -> string

(** The input signature of the functor [Configuration_parser.Make]. *)
module type Definition =
sig
  type t
  (** The type of parser state. *)

  (** Receive a comment. *)
  val comment : t -> excerpt -> unit

  (** Receive a section specification. *)
  val section : t -> excerpt list -> unit

  (** Receive a binding specificaton. *)
  val binding : t -> excerpt -> excerpt -> unit

  (** Receive a parser error. *)
  val parse_error : t -> pos -> error -> unit

end

(** The output signature of the functor [Configuration_parser.Make]. *)
module type S =
sig

  (** The type of parser state. *)
  type t

  (** Parse the given lexing buffer stream. *)
  val parse : t -> Lexing.lexbuf -> unit

  (** Parse the given string. *)
  val parse_string : t -> string -> unit

  (** Parse the given input channel. *)
  val parse_channel : t -> in_channel -> unit

  (** Parse the given file.

  @raise Sys_error if the file cannot be opened. *)
  val parse_file : t -> string -> unit

end


module Make(D:Definition): S
  with type t = D.t
