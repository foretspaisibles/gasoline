(* WSGML -- Writer for SGML files

Author: Michael Grünewald
Date: Sat 21 Feb 2009 09:21:26 CET

Gasoline (https://github.com/michipili/gasoline)
This file is part of Gasoline

Copyright © 2009–2014 Michael Grünewald

This file must be used under the terms of the CeCILL-B.
This source file is licensed as described in the file COPYING, which
you should have received as part of this distribution. The terms
are also available at
http://www.cecill.info/licences/Licence_CeCILL-B_V1-en.txt *)
open Unicode
open UFormat

let sgml_indent = 2

(* Some constants. *)

let buffer_sz = 4096	(* Random buffer size *)
let margin_sz = 2 	(* Margins in the pretty printer *)

let amp = UChar.of_char '&'
let lt = UChar.of_char '<'
let gt = UChar.of_char '>'
let quot = UChar.of_char '"'

let esc_amp = u"&amp;"
let esc_lt = u"&lt;"
let esc_gt = u"&gt;"
let esc_quot = u"&quot;"
let uempty = u""

type cdata = ustring
type pcdata = ustring
type nmtoken = string

let escape_loop output_ustring output_uchar s =
  let escape_char c =
    if c = amp then
      output_ustring esc_amp
    else if c = lt then
      output_ustring esc_lt
    else if c = gt then
      output_ustring esc_gt
    else if c = quot then
      output_ustring esc_quot
    else
      output_uchar c
  in
  UString.iter escape_char s

let escape s =
  let l = UString.length s in
  let b = UBuffer.create (3*l) in
    escape_loop (UBuffer.add_ustring b) (UBuffer.add_uchar b) s;
    UBuffer.contents b


type snippet =
  | CDATA of cdata
  | PCDATA of pcdata
  | ELEMENT of element

and element  = {
  name : nmtoken;
  attribute : (nmtoken * cdata) Queue.t;
  empty : bool;
  block : bool;
  content : snippet Queue.t;
}

let pcdata s =
  PCDATA s

let cdata s =
  CDATA s

let enqueue q l =
  List.iter (fun x -> Queue.add x q) l

let make_element n ?(empty = false) ?(block = true) ?(attr = []) c =
  if n = "" then invalid_arg "WSGML.element";
  let e = {
    name = n;
    attribute = Queue.create();
    empty = empty;
    block = block;
    content = Queue.create();
  } in
    enqueue e.attribute attr;
    enqueue e.content c;
    e

let element n ?empty ?block ?attr c =
  ELEMENT(make_element n ?empty ?block ?attr c)

type t = {
  declaration: cdata;
  dtd: cdata;
  snips: snippet list;
}

let make ?(declaration = "") ?(dtd = "") content = {
  declaration = u declaration;
  dtd = u dtd;
  snips = content;
}

let rec pp_print_snippet_loop f h d =
  let put_uchar x =
    if UInformation.is_space x then
      pp_print_space f ()
    else
      pp_print_uchar f x
  in
  let put_ustring x =
    UString.iter put_uchar x in
  let pp_print_pcdata _ x = escape_loop put_ustring put_uchar x in
  let pp_print_attribute f (k,v) =
    begin
      pp_print_space f ();
      pp_open_hbox f ();
      pp_print_string f k;
      pp_print_char f '=';
      pp_print_char f '"';
      pp_print_ustring f v;
      pp_print_char f '"';
      pp_close_box f ();
    end
  in

  let pp_print_element_open_tag f h e =
    begin
      pp_open_hvbox f 0;
      pp_open_hvbox f sgml_indent;
      pp_print_char f '<';
      pp_print_string f e.name;
      Queue.iter (pp_print_attribute f) e.attribute;
      pp_close_box f ();
      pp_print_cut f ();
      pp_print_char f '>';
      pp_close_box f ();
    end
  in
  let pp_print_element_close_tag f h e =
    begin
      pp_open_hbox f ();
      pp_print_char f '<';
      pp_print_char f '/';
      pp_print_string f e.name;
      pp_print_char f '>';
      pp_close_box f ();
    end
  in

  let pp_print_element_content f h e =
    begin
      Queue.iter (pp_print_snippet_loop f e.block) e.content;
    end
  in
  let pp_print_element f h e =
    begin
      if h then pp_print_cut f ();
      if e.block || h then (
	pp_open_hbox f ();
	pp_open_vbox f 0;
      );

      (
	if e.block then
	  pp_open_vbox f sgml_indent
	else if h then
	  pp_open_hovbox f sgml_indent
      );
      pp_print_element_open_tag f h e;
      if not e.empty then (
	if e.block then pp_print_cut f ();
	pp_print_element_content f h e;
	if e.block then pp_print_cut f ();
      );
      if e.block || h then pp_close_box f ();
      if e.block then pp_print_cut f ();
      if e.block || h then pp_close_box f ();
      if not e.empty then (
	pp_print_element_close_tag f h e;
      );
      if h then pp_print_cut f ();
      if e.block || h then pp_close_box f ();
      if h then pp_print_cut f ();
    end
  in
  match d with
    | PCDATA c -> pp_print_pcdata f c
    | CDATA c -> pp_print_ustring f c
    | ELEMENT e -> pp_print_element f h e



let uformat f d =
  let pp_maybe_print_uline f l =
    if not (l = uempty) then
      begin
	pp_open_hbox f ();
	pp_print_ustring f l;
	pp_close_box f ();
	pp_print_cut f ();
      end
  in
  let pp_print_snippet f s =
    begin
      pp_open_hbox f ();
      pp_print_snippet_loop f false s;
      pp_close_box f ();
      pp_print_cut f ();
    end
  in
  begin
    pp_open_vbox f 0;
    pp_maybe_print_uline f d.declaration;
    pp_maybe_print_uline f d.dtd;
    List.iter (pp_print_snippet f) d.snips;
    pp_close_box f ();
  end

let format ?enc f d =
  uformat (formatter_of_formatter ?enc f) d

let add_to_buffer ?enc b d =
  uformat (formatter_of_buffer ?enc b) d

let to_buffer ?enc d =
  let b = Buffer.create 4096 in
  begin
    add_to_buffer ?enc b d;
    b;
  end

let to_string ?enc d =
  Buffer.contents (to_buffer ?enc d)

let output ?enc c d =
  uformat (formatter_of_out_channel ?enc c) d

let print ?enc d =
  output ?enc stdout d

let write ?enc f d =
  let c = open_out f in
  begin
    output ?enc c d;
    close_out c;
  end

let concat = List.concat


let rec outcome_loop x l =
  match l with
    | (_, None)::t -> outcome_loop x t
    | (f, Some v)::t -> outcome_loop ((f v)::x) t
    | [] -> List.rev x

let outcome l =
  outcome_loop [] l


type factory = element

let prepare x =
  match x with
    | ELEMENT e -> e
    | _ -> invalid_arg "prepare"

let add_attribute f a =
  Queue.add a f.attribute

let add_content f c =
  Queue.add c f.content

let finalize f =
  ELEMENT f
