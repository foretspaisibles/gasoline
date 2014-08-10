(* Configuration -- Generic configuration facility

Author: Michael Grünewald
Date: Wed Oct 24 07:48:50 CEST 2012

Copyright © 2012 Michael Grünewald

This file must be used under the terms of the CeCILL-B.
This source file is licensed as described in the file COPYING, which
you should have received as part of this distribution. The terms
are also available at
http://www.cecill.info/licences/Licence_CeCILL-B_V1-en.txt *)

(* Finite automatons recognising globbing patterns. *)
module Glob =
struct

  module State =
  struct
    type t = int
    let compare = Pervasives.compare
  end

  module State_set =
    Set.Make(State)

  type t = string

  let add_raw_transition patt c state a =
    let patt_sz = String.length patt in
    if state < patt_sz then (
      match patt.[state] with
      | '?' -> State_set.add (state + 1) a
      | '*' -> State_set.add state a
      | p -> if p = c then State_set.add (state + 1) a else a
    ) else (
      a
    )

  let add_epsilon_transition patt state a =
    let patt_sz = String.length patt in
    if state < patt_sz && patt.[state] = '*' then
      State_set.add (state + 1) a
    else
      a

  let raw_transition patt c a =
    State_set.fold (add_raw_transition patt c) a State_set.empty

  let rec saturate patt a =
    let epsilon =
      State_set.fold (add_epsilon_transition patt) a State_set.empty
    in
    if State_set.subset epsilon a then
      a
    else
      saturate patt (State_set.union a epsilon)

  let transition patt c a =
    saturate patt (raw_transition patt c a)

  let iteration patt active c =
    active := transition patt c !active

  (* [string_match patt s] recognises the string [s] matching [patt]. *)
  let string_match patt s =
    let active = ref(saturate patt (State_set.singleton 0)) in
    begin
      String.iter (iteration patt active) s;
      State_set.mem (String.length patt) !active
    end
end


exception Parse_error of Lexing.position * string

let parse_error pos s =
  raise (Parse_error(pos,s))

(* We implement configuration sets as a functor parametrised by
messages emitted on the occurence of various events. *)

module type MESSAGE =
sig
  val value_error : string list -> string -> Lexing.position -> string -> unit
  val parse_error : Lexing.position -> string -> unit
  val default : string list -> string -> string -> unit
  val uncaught_exn : string list -> string -> exn -> unit
end

module type S =
sig
  type t
  type 'a concrete = {
    of_string: string -> 'a;
    to_string: 'a -> string;
  }
  type 'a key = {
    concrete: 'a concrete;
    path: string list;
    name: string;
    default: 'a;
    description: string;
  }
  type handler
  val handler : 'a key -> ('a -> unit) -> handler
  val key : ('a concrete) -> string list -> string -> 'a -> string -> 'a key
  val get : t -> 'a key -> 'a
  val apply : t -> handler -> unit
  val value : 'a key -> string -> 'a
  val empty : t
  val add : t -> (string list * string) -> string -> t
  val merge : t -> t -> t
  val override : t -> t -> t
  val from_file : string -> t
  val from_string : string -> t
  val from_alist : ((string list * string) * string) list -> t
end

(* We provide a simple implementation of the required associative
structure based on alists.

An implementation based on finite automatons could be interesting in
the case where there is a large number of keys, because it would speed
up the retrieval.

It is not possible to use an hashtable because keys could be patterns. *)
module Make(M:MESSAGE) =
struct

  let path_to_string p k =
    String.concat "." (p @ [k])

  type t =
    (string * string) list

  type 'a concrete = {
    of_string: string -> 'a;
    to_string: 'a -> string;
  }

  type 'a key = {
    concrete: 'a concrete;
    path: string list;
    name: string;
    default: 'a;
    description: string;
  }

  type handler = {
    handler_path: string list;
    handler_name: string;
    handler_description: string;
    handler_callback: t -> unit;
  }

  let key c p k def des = {
    concrete = c;
    path = p;
    name = k;
    default = def;
    description = des;
  }

  let assoc key a =
    let path_as_string =
      path_to_string key.path key.name
    in
    let string_match (glob, data) =
      Glob.string_match glob path_as_string
    in
    snd (List.find string_match a)

  let use_default key =
    M.default key.path key.name (key.concrete.to_string key.default);
    key.default

  let value key text =
    try key.concrete.of_string text
    with
    | Parse_error(pos, s) -> (
      M.value_error key.path key.name pos s;
      use_default key
    )
    | exn -> (
      M.uncaught_exn key.path key.name exn;
      use_default key
    )

  let get a key =
    try value key (assoc key a)
    with
    | Not_found -> use_default key

  let handler key cb =
    let handler_callback conf =
      cb (get conf key)
    in
    {
      handler_path = key.path;
      handler_name = key.name;
      handler_description = key.description;
      handler_callback;
    }

  let apply conf handler =
    handler.handler_callback conf

  let empty = []

  let add a (p,k) v =
    (path_to_string p k,v) :: a

  let merge a b =
    a @ b

  let rec override_loop a b ax =
    match a with
    | [] -> List.rev ax
    | (k,v)::t -> (
      if List.mem_assoc k b then
	override_loop t b ((k, List.assoc k b) :: ax)
      else
	override_loop t b ((k,v) :: ax)
    )

  let override a b =
    override_loop a b []


  (* Definition of our configuration parser *)
  module Parser_definition =
  struct
    type configuration = t

    type t = {
      mutable path: string list;
      mutable conf: configuration;
    }

    let comment _ _ = ()

    let section p l =
      p.path <- List.map Configuration_parser.text l

    let binding p k v =
      p.conf <- (
	Configuration_parser.text k,
	Configuration_parser.text v
      ) :: p.conf

    let parse_error p pos error =
      M.parse_error pos (Configuration_parser.error_to_string error)

  end

  module Parser = Configuration_parser.Make(Parser_definition)

  let from_anything f x =
    let p = {
      Parser_definition.
      path = [];
      conf = [];
    } in
    begin
      f p x;
      List.rev p.Parser_definition.conf
    end

  let from_file =
    from_anything Parser.parse_file

  let from_string =
    from_anything Parser.parse_string

  let from_alist a =
    let loop c (k,v) = add c k v in
    List.fold_left loop empty a
end


module Quiet =
struct

  let value_error path name pos value =
    ()

  let parse_error pos message =
    ()

  let default path name value =
    ()

  let uncaught_exn exn path name =
    ()
end


module Internal =
  (Make(Quiet) : S)

include Internal
