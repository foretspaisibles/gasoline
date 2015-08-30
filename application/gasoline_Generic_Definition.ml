(* Gasoline_Generic_Type -- Definition of generic types

Author: Michael Grünewald
Date: Wed May 22 07:04:55 CEST 2013

Copyright © 2013 Michael Grünewald

This file must be used under the terms of the CeCILL-B.
This source file is licensed as described in the file COPYING, which
you should have received as part of this distribution. The terms
are also available at
http://www.cecill.info/licences/Licence_CeCILL-B_V1-en.txt *)
module type DATA =
sig
  type word
  type block
  type out_channel
  val output_word : out_channel -> word -> unit
  val output_block : out_channel -> block -> int -> int -> unit
  val flush : out_channel -> unit
end

module type BUFFER =
sig
  type word
  type block
  type out_channel
  type t
  val create : int -> t
  val contents : t -> block
  val nth : t -> int -> word
  val length : t -> int
  val clear : t -> unit
  val reset : t -> unit
  val add_word : t -> word -> unit
  val add_block : t -> block -> unit
  val add_buffer : t -> t -> unit
  val output_buffer : out_channel -> t -> unit
end

module type VALUE =
sig
  type t
  type locale
  type format
  val stdformat : format
  val format_to_string : format -> string
  val format_of_string : string -> format
  type buffer
  val add_formatted : buffer -> locale -> buffer -> format -> t -> unit
  val add : buffer -> locale -> buffer -> t -> unit
  val add_text : buffer -> locale -> buffer -> string -> unit
end

module type MESSAGEDB =
sig
  type t
  type connection_token
  val opendb : connection_token -> t
  val find : t -> string -> string
  val close : t -> unit
end

module type PRIORITY =
sig
  type t
  val compare : t -> t -> int
  type locale
  type buffer
  val add_pre : locale -> buffer -> t -> string -> unit
  val add_post : locale -> buffer -> t -> string -> unit
  type out_channel
  val send_pre : locale -> out_channel -> t -> string -> unit
  val send_post : locale -> out_channel -> t -> string -> unit
end

module type LOCALE =
sig
  type t
  val stdlocale : t
end
