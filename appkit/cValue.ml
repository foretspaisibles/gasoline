(* CValue -- Dynamically typed values for plain applications

Author: Michael Grünewald
Date: Thu Jul 10 11:02:19 CEST 2014

Gasoline (https://github.com/michipili/gasoline)
This file is part of Gasoline

Copyright © 2014 Michael Grünewald

This file must be used under the terms of the CeCILL-B.
This source file is licensed as described in the file COPYING, which
you should have received as part of this distribution. The terms
are also available at
http://www.cecill.info/licences/Licence_CeCILL-B_V1-en.txt *)
open Scanf

let ident x = x

type 'a kind =
| Bool : bool kind
| Int : int kind
| Char : char kind
| String : string kind
| Float : float kind

type (_,_) witness =
| Witness : ('a,'a) witness

let kind_eq :
type a b. a kind -> b kind -> (a,b) witness option =
  fun a b -> match a, b with
  | Bool, Bool -> Some Witness
  | Int, Int -> Some Witness
  | Char, Char -> Some Witness
  | String, String -> Some Witness
  | Float, Float -> Some Witness
  | _ -> None

let kind_nmb :
type a. a kind -> int =
  fun a -> match a with
  | Bool -> 0
  | Int -> 1
  | Char -> 2
  | String -> 3
  | Float -> 4

type t =
| Thingie : 'a kind * 'a -> t

let get_option :
type a. a kind -> t -> a option =
  fun k1 (Thingie(k2, v)) -> match kind_eq k1 k2 with
  | Some Witness -> Some v
  | None -> None

let get k v =
  match get_option k v with
  | Some(x) -> x
  | None -> failwith "CValue.get"

let make k v =
  Thingie(k, v)

let has_kind :
type a. a kind -> t -> bool =
  fun k1 (Thingie(k2,_)) -> match kind_eq k1 k2 with
  | Some(Witness) -> true
  | None -> false

let same_kind (Thingie(k1,_)) (Thingie(k2,_)) =
  match kind_eq k1 k2 with
  | Some(_) -> true
  | None -> false

let cast :
type a b. (a,b) witness -> a -> b =
  fun Witness(x) -> x

let equal c1 c2 =
  match c1, c2 with
  | Thingie(Bool, v1), Thingie(Bool, v2) -> v1 = v2
  | Thingie(Int, v1), Thingie(Int, v2) -> v1 = v2
  | Thingie(Char, v1), Thingie(Char, v2) -> v1 = v2
  | Thingie(String, v1), Thingie(String, v2) -> v1 = v2
  | Thingie(Float, v1), Thingie(Float, v2) -> v1 = v2
  | _ -> false

let compare c1 c2 =
  match c1, c2 with
  | Thingie(Bool, v1), Thingie(Bool, v2) -> Pervasives.compare v1 v2
  | Thingie(Int, v1), Thingie(Int, v2) -> Pervasives.compare v1 v2
  | Thingie(Char, v1), Thingie(Char, v2) -> Pervasives.compare v1 v2
  | Thingie(String, v1), Thingie(String, v2) -> Pervasives.compare v1 v2
  | Thingie(Float, v1), Thingie(Float, v2) -> Pervasives.compare v1 v2
  | Thingie(k1,_), Thingie(k2,_) -> (kind_nmb k1) - (kind_nmb k2)

(* Remark: it is possible to test for a character without reading it
by using the reader format "%0c".  It is therefore possible to
implement decisions within readers. *)

let reader_string c =
  let select = function
    | '"' -> bscanf c "%S"
    | _ -> bscanf c "%s"
  in
  bscanf c "%0c" select

let reader_kind :
type a. a kind -> Scanf.Scanning.in_channel -> a =
  fun k c -> match k with
  | Bool -> bscanf c "%B" ident
  | Int -> bscanf c "%i" ident
  | Char -> bscanf c "%C" ident
  | String -> reader_string c ident
  | Float -> bscanf c "%f" ident

let reader_same (Thingie(k,_)) c =
  Thingie(k,reader_kind k c)

let of_string_kind :
type a. a kind -> string -> a =
  fun k s ->
    try sscanf s "%r%!" (reader_kind k) ident
    with Scan_failure(_) -> failwith "CValue.of_string_kind"

let of_string_same (Thingie(k,_)) s =
  try Thingie(k,of_string_kind k s)
  with Failure(_) -> failwith "CVaule.of_string_same"

let taste :
type a. a kind -> string -> bool =
  fun k s ->
    try (ignore (of_string_kind k s); true)
    with Failure(_) -> false

let maybe_convert k s =
  try Some(Thingie(k, of_string_kind k s))
  with Failure(_) -> None

let of_string s =
  let converter_list = [
    maybe_convert Bool;
    maybe_convert Int;
    maybe_convert Char;
    maybe_convert Float;
    maybe_convert String;
  ] in
  let rec loop l =
    match l with
    | [] -> failwith "CValue.of_string"
    | hd :: tl -> (match hd s with Some x -> x | None -> loop tl)
  in
  loop converter_list

let to_string v = match v with
  | Thingie(Bool, b) -> string_of_bool b
  | Thingie(Int, x) -> string_of_int x
  | Thingie(Char, c) -> String.make 1 c
  | Thingie(String, s) -> s
  | Thingie(Float, f) -> string_of_float f

let printer ppt v =
  let open Format in
  match v with
  | Thingie(Bool, b) -> fprintf ppt "Bool(%B)" b
  | Thingie(Int, x) -> fprintf ppt "Int(%d)" x
  | Thingie(Char, c) -> fprintf ppt "Char(%C)" c
  | Thingie(String, s) -> fprintf ppt "String(%S)" s
  | Thingie(Float, f) -> fprintf ppt "Float(%g)" f
