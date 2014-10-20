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
open Format


module type S =
sig
  type t
  val empty : t
  val add_word : t -> string -> t
  val add_list : t -> string list -> t
  val add_stream : t -> string Stream.t -> t
  val add_file : t -> string -> t
  type state
  val initial : state
  val outcome : t -> state -> char * state
  val format : Format.formatter -> t -> unit
  val to_persistant : t -> Persistant.t
  val of_persistant : Persistant.t -> t
end


module type P =
sig
  val length : int
end


module Symbol =
struct

  let valid_char =
    "\000abcdefghijklmnopqrstuvwxyz-"

  let special =
    '\000'

  let size =
    String.length valid_char

  type t =
    char

  let validate c =
    try ignore (String.index valid_char c); true
    with Not_found -> false

  let is_special c =
    c = special

  let word w =
    let q = Queue.create () in
    let rec loop c =
      Queue.add c q
    in
    begin
      String.iter loop w;
      List.rev (Queue.fold (fun ax c -> c :: ax) [] q)
    end

  let format fft c =
    Format.pp_print_char fft (if is_special c then '*' else c)

end


module Transition =
struct

  type t = {
    weight: int;
    outcome: (int * Symbol.t) list;
  }

  let empty = {
    weight = 0;
    outcome = [];
  }

  let add t s =
    let weight = t.weight + 1 in
    let rec loop a l =
	match l with
	| [] ->
	  { weight; outcome = (1,s) :: a; }
	| ((w,u) as head) :: tail ->
	  if u = s then
	    { weight; outcome = a @ ((w+1,u) :: tail); }
	  else
	    loop (head :: a) tail
    in
    loop [] t.outcome

  let choose t =
    let n = Random.int t.weight in
    let rec loop ax l =
      match l with
      | (w,s) :: tail ->
	let ax' = w + ax in
	if ax' > n then
	  s
	else
	  loop ax' tail

      | _ -> failwith "Transition.choose"
	(* This only happens if the weight of our structure is wrong
	   or the prng is broken. *)

    in loop 0 t.outcome

  let rec format fft t =
    fprintf fft "@[<b0>{@[<hv2>@ weight = %d;@ @[<hov2>outcome = %a@]@]@ }@]" t.weight
      format_outcome t.outcome
  and format_outcome fft l =
    match l with
    | (n,s) :: tail -> fprintf fft "@[(%d,@ %a)@];@ %a" n Symbol.format s format_outcome tail
    | [] -> ()

end


module Make (P:P) =
struct

  module State =
  struct

    type t =
      Symbol.t array

    let length = P.length - 1
    let initial = Array.make length Symbol.special
    let compare = Pervasives.compare

    let padding =
      Array.to_list initial

    let padded_word w =
      (Symbol.word w) @ [ Symbol.special ]

    let next state s =
      let state' = Array.copy state in
      begin
	for i = 0 to length - 2 do
	  state'.(i) <- state'.(i+1);
	done;
	state'.(length - 1) <- s;
	state'
      end

    let format fft state =
      begin
	fprintf fft "@[<2>\"";
	Array.iter (Symbol.format fft) state;
	fprintf fft "\"@]"
      end

  end

  module StateMap =
    Map.Make(State)

  type t =
    Transition.t StateMap.t

  let empty =
    StateMap.empty

  let add m (state,s) =
    let t =
      try StateMap.find state m
      with Not_found -> Transition.empty
    in
    StateMap.add state (Transition.add t s) m

  let add_word m word =
    let pword = State.padded_word word in
    let rec loop (m, state) l =
      match l with
      | [] -> (m, state)
      | h :: t -> loop (add m (state, h), State.next state h) t
    in
    fst(loop (m, State.initial) pword)

  let add_list m l =
    List.fold_left add_word m l

  let add_stream m s =
    let ax = ref m in
    let loop w =
      ax := add_word !ax w
    in
    begin
      Stream.iter loop s;
      !ax
    end

  let add_file m name =
    let c = open_in name in
    let f _ =
      try Some(input_line c)
      with End_of_file -> None
    in
    let answer =
      add_stream m (Stream.from f)
    in
    begin
      close_in c;
      answer
    end

  type state =
    State.t

  let initial =
    State.initial

  let outcome m state =
    let c =
      try Transition.choose (StateMap.find state m)
      with Not_found -> Symbol.special
    in
    (c, State.next state c)

  let rec format fft m =
    let loop k v =
      fprintf fft "@[<b0>{ %a => %a@ }@]@." State.format k Transition.format v
    in
    begin
      fprintf fft "Map{@[<b 2>@.";
      StateMap.iter loop m;
      fprintf fft "@]}";
    end

  type alist =
    (State.t * Transition.t) list

  let to_alist m =
    let loop key v ax =
      (key,v) :: ax
    in
    StateMap.fold loop m []

  let of_alist a =
    let loop m (k,v) =
      StateMap.add k v m
    in
    List.fold_left loop StateMap.empty a

  let to_persistant m =
    Marshal.to_string m []

  let of_persistant s =
    Marshal.from_string s 0
end
