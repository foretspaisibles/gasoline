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

module type P =
sig
  val length : int
  val generate_min : int
  val generate_max : int
end

module type S =
sig
  type t
  type table
  val create : table -> t
  val generate : t -> string
end

let buffer_sz = 128


module Make(P:P) =
struct

  module Internal_table = Table.Make(P)

  type t = {
    buf: Buffer.t;
    tbl: Internal_table.t;
  }

  type table =
    Internal_table.t

  let create tbl = {
    buf = Buffer.create buffer_sz;
    tbl
  }

  let generate_inner a =
    let rec loop state =
      let (s, next) = Internal_table.outcome a.tbl state in
      if s = '\000' then
	Buffer.contents a.buf
      else
	(Buffer.add_char a.buf s; loop next)
    in
    begin
      Buffer.clear a.buf;
      loop Internal_table.initial
    end

  let rec generate a =
    let candidate = generate_inner a in
    let candidate_sz = String.length candidate in
    if candidate_sz >= P.generate_min && candidate_sz <= P.generate_max
    then
      candidate
    else
      generate a

end
