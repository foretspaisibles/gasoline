(* Gasoline_Author_SGML -- Writer for SGML files

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
open Gasoline_Unicode
open Format

(* Some boxes *)
let withbox openbox ppt a f =
  begin
    openbox ppt a;
    f ppt;
    pp_close_box ppt ();
  end

let withhbox ppt f =
  withbox pp_open_hbox ppt () f

let withvbox ppt indent f =
  withbox pp_open_vbox ppt indent f

let withhvbox indent ppt f =
  withbox pp_open_hvbox ppt indent f

let withhovbox indent ppt f =
  withbox pp_open_hovbox ppt indent f


(* Some constants. *)

let buffer_sz = 4096	(* Random buffer size *)
let indent_sz = 2 	(* Margins in the pretty printer *)

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
  empty : bool;
  block : bool;
  attribute : (nmtoken * cdata) list;
  content : snippet list;
}

let pcdata s =
  PCDATA s

let cdata s =
  CDATA s

let enqueue q l =
  List.iter (fun x -> Queue.add x q) l

let make_element n ?(empty = false) ?(block = true) ?(attr = []) c =
  if n = "" then invalid_arg "WSGML.element" else {
    name = n;
    attribute = attr;
    empty = empty;
    block = block;
    content = c;
  }

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

let put_uchar enc ppt x =
  if UInformation.is_space x then
    pp_print_space ppt ()
  else
    UChar.format enc ppt x

let put_ustring enc ppt x =
  UString.iter (put_uchar enc ppt) x

let pp_print_pcdata enc ppt x =
  escape_loop (put_ustring enc ppt) (put_uchar enc ppt) x

let pp_print_attribute enc ppt (k,v) =
  let pp_print_binding enc ppt =
    pp_print_string ppt k;
    pp_print_char ppt '=';
    pp_print_char ppt '"';
    UString.format enc ppt v;
    pp_print_char ppt '"';
  in
  begin
    pp_print_space ppt ();
    withhbox ppt (pp_print_binding enc);
  end


let pp_print_element_open_tag enc ppt e =
  begin
    pp_open_hvbox ppt 0;
    pp_open_hvbox ppt indent_sz;
    pp_print_char ppt '<';
    pp_print_string ppt e.name;
    List.iter (pp_print_attribute enc ppt) e.attribute;
    pp_close_box ppt ();
    pp_print_cut ppt ();
    pp_print_char ppt '>';
    pp_close_box ppt ();
  end


let pp_print_element_close_tag ppt e =
  begin
    pp_open_hbox ppt ();
    pp_print_char ppt '<';
    pp_print_char ppt '/';
    pp_print_string ppt e.name;
    pp_print_char ppt '>';
    pp_close_box ppt ();
  end

let pp_print_nonempty_element f enc ppt e =
  let cutonlyforblocks () =
    if e.block then pp_print_cut ppt ()
  in
  let pack x =
    cutonlyforblocks();
    f enc ppt x
  in
  begin
    pp_open_hvbox ppt 0;
    (if e.block then
	pp_open_vbox ppt indent_sz
     else
	pp_open_hovbox ppt 0
    );
    pp_print_element_open_tag enc ppt e;
    List.iter pack e.content;
    pp_close_box ppt ();
    if e.block then pp_force_newline ppt ();
    pp_print_element_close_tag ppt e;
    pp_close_box ppt ();
  end


let pp_print_element f enc ppt e =
  if e.empty then
    pp_print_element_open_tag enc ppt e
  else
    pp_print_nonempty_element f enc ppt e

let rec pp_print_snippet_loop enc ppt d =
  match d with
  | PCDATA c -> pp_print_pcdata enc ppt c
  | CDATA c -> UString.format enc ppt c
  | ELEMENT e -> pp_print_element pp_print_snippet_loop enc ppt e



let format ?(enc = Encoding.locale) ppt d =
  let pp_maybe_print_uline enc ppt l =
    if not (l = uempty) then
      begin
	pp_open_hbox ppt ();
	UString.format enc ppt l;
	pp_close_box ppt ();
	pp_print_cut ppt ();
      end
  in
  let pp_print_snippet enc ppt s =
    begin
      pp_open_hbox ppt ();
      pp_print_snippet_loop enc ppt s;
      pp_close_box ppt ();
      pp_print_cut ppt ();
    end
  in
  begin
    pp_open_vbox ppt 0;
    pp_maybe_print_uline enc ppt d.declaration;
    pp_maybe_print_uline enc ppt d.dtd;
    List.iter (pp_print_snippet enc ppt) d.snips;
    pp_close_box ppt ();
  end

let add_to_buffer ?enc b d =
  format ?enc (formatter_of_buffer b) d

let to_buffer ?enc d =
  let b = Buffer.create 4096 in
  begin
    add_to_buffer ?enc b d;
    b;
  end

let to_string ?enc d =
  Buffer.contents (to_buffer ?enc d)

let output ?enc c d =
  let ppt = formatter_of_out_channel c in
  format ?enc ppt d;
  pp_print_flush ppt ()

let print ?enc d =
  output ?enc stdout d

let write ?enc ppt d =
  let c = open_out ppt in
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
