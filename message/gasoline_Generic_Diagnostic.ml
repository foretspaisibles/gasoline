(* Gasoline_Generic_Diagnostic -- Generic diagnostic facility

Author: Michael Grünewald
Date: Mon Oct 15 21:15:26 CEST 2012

Copyright © 2012 Michael Grünewald

This file must be used under the terms of the CeCILL-B.
This source file is licensed as described in the file COPYING, which
you should have received as part of this distribution. The terms
are also available at
http://www.cecill.info/licences/Licence_CeCILL-B_V1-en.txt *)
open Gasoline_Generic_Definition


module type FACILITY =
sig
  type t
end


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

  module MessageDB : MESSAGEDB

  module Priority : PRIORITY
    with type locale = Value.locale
     and type buffer = Buffer.t
     and type out_channel = Data.out_channel

  module Facility : FACILITY
end


module type S =
sig
  type t
  val create : unit -> t
  type sink
  type facility
  val add : t -> facility -> sink -> unit
  val remove : t -> facility -> unit
  val mem : t -> facility -> bool
  val iter : (facility -> sink -> unit) -> t -> unit
  val fold : (facility -> sink -> 'a -> 'a) -> t -> 'a -> 'a
  type priority
  type value
  val send : t -> facility -> priority -> string -> (string * value) list -> unit
end


module Make(P:P) =
struct

  (* The expected size of the table *)
  let table_sz = 31

  module Sink = Gasoline_Generic_Message.Sink.Make(P)

  module Table = Hashtbl.Make(
      struct
        type t = P.Facility.t
        let hash = Hashtbl.hash
        let equal = (=)
      end
  )

  type sink = Sink.t
  type facility = P.Facility.t
  type priority = P.Priority.t
  type value = P.Value.t

  type t = sink Table.t

  let fold = Table.fold
  let iter = Table.iter
  let mem = Table.mem
  let remove = Table.remove
  let add = Table.add
  let create () = Table.create table_sz

  let send x f cls id bindings =
    let sink_send s =
      Sink.send s cls id bindings
    in
    List.iter sink_send (Table.find_all x f)
end
