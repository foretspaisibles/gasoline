(* WHTML -- Writer for HTML files

Author: Michael Grünewald
Date: Sat 21 Feb 2009 10:36:03 CET

Gasoline (https://github.com/michipili/gasoline)
This file is part of Gasoline

Copyright © 2009–2014 Michael Grünewald

This file must be used under the terms of the CeCILL-B.
This source file is licensed as described in the file COPYING, which
you should have received as part of this distribution. The terms
are also available at
http://www.cecill.info/licences/Licence_CeCILL-B_V1-en.txt *)
open Unicode
include WSGML

let uspace = u" "
let prepare_id x = x
let prepare_cdata_list x = [cdata x]
let prepare_unit () = []

type html_attribute =
  | NMTOKEN of nmtoken option
  | CDATA of cdata option
  | NMTOKEN_LIST of nmtoken list option
  | INTEGER of int option

let html_attribute_add (n,o) l =
  match o with
    | (NMTOKEN None | CDATA None | NMTOKEN_LIST None | INTEGER None) -> l
    | NMTOKEN(Some v) -> (n,u v) :: l
    | CDATA(Some v) -> (n, v) :: l
    | NMTOKEN_LIST(Some v) -> (n, UString.join uspace (List.map u v)) :: l
    | INTEGER(Some k) -> (n, u(string_of_int k))::l

let html_attribute l1 l2 =
  List.fold_right html_attribute_add l1 (
    match l2 with
      | None -> []
      | Some a -> a
  )

let html_element_simple name prepare_content
    ?empty ?block ?id ?cls ?lang ?dir ?style ?title ?attr c =
  element name ?empty ?block
    ~attr:(html_attribute [
	     "style", CDATA style;
	     "class", NMTOKEN cls;
	     "lang", NMTOKEN lang;
	     "dir", NMTOKEN dir;
	     "title", CDATA title;
	     "id", NMTOKEN id;
	   ] attr) (prepare_content c)

let html_element_structure name =
  html_element_simple name prepare_id ~empty:false ~block:true

let html_element_empty_structure name =
  html_element_simple name prepare_id ~empty:true ~block:true

let html_element_text name =
  html_element_simple name prepare_cdata_list ~empty:false ~block:false

let html_element_empty_text name =
  html_element_simple name prepare_unit ~empty:true ~block:false

let html_element_hier name =
  html_element_simple name prepare_cdata_list ~empty:false ~block:false ~attr:[]

let html_element_block name =
  html_element_simple name prepare_id ~empty:false ~block:true ~attr:[]

let html_element_paragraph name =
  html_element_simple name prepare_id ~empty:false ~block:false ~attr:[]

let html_element_markup name =
  html_element_text name ~attr:[]


let meta ?lang ?dir ?http_equiv ?name ?scheme c =
  html_element_empty_structure "meta"
    ~attr:(
      html_attribute [
	"name", NMTOKEN name;
	"http-equiv", NMTOKEN http_equiv;
	"lang", NMTOKEN lang;
	"dir", NMTOKEN dir;
	"scheme", NMTOKEN scheme;
      ] (Some ["content", c])
    ) []

let meta_simple name ?lang ?dir c =
  meta ~name ?lang ?dir c

let author = meta_simple "author"
let generator = meta_simple "generator"
let copyright = meta_simple "copyright"
let keywords ?lang ?dir l =
  meta_simple "copyright" ?lang ?dir (UString.join (u",") l)
let date = meta_simple "date"

let link ?id ?cls ?title ?style ?lang ?dir ?charset ?hreflang
    ?rel ?rev ?typ href =
  html_element_empty_structure "link"
    ~attr:(
      html_attribute [
	"id", NMTOKEN id;
	"class", NMTOKEN cls;
	"title", CDATA title;
	"style", CDATA style;
	"lang", NMTOKEN lang;
	"dir", NMTOKEN dir;
	"charset", NMTOKEN charset;
	"hreflang", NMTOKEN hreflang;
	"rel", NMTOKEN_LIST rel;
	"rev", NMTOKEN_LIST rev;
	"type", NMTOKEN typ;
	"href", NMTOKEN (Some href);
      ] None
    ) []

let css_persistent ?id ?cls ?style href =
  link ?id ?cls ?style ~rel:["stylesheet"] ~typ:"text/css" href

let css_alternate ?id ?cls ?style ~title href =
  link ?id ?cls ?style ~rel:["alternate"; "stylesheet"] ~typ:"text/css"
    ~title href

let css_preferred ?id ?cls ?style ~title href =
  link ?id ?cls ?style ~rel:["stylesheet"] ~typ:"text/css"
    ~title href

let style ?lang ?dir ?typ ?title content =
  html_element_structure "style" ~attr:(
    html_attribute [
	"lang", NMTOKEN lang;
	"dir", NMTOKEN dir;
	"type", NMTOKEN typ;
	"title", CDATA title;
    ] None
  ) [cdata content]


let css_inline_persistent c = style ~typ:"text/css" c

let css_inline_preferred ?lang ?dir ~title c =
  style ?lang ?dir ~typ:"text/css" ~title c

let default_style ?lang ?dir title =
  meta ~http_equiv:"Default-Style" ?lang ?dir title

let link_simple rel ?id ?cls ?title ?style ?lang ?dir ?charset ?hreflang ?rev href =
  link ?id ?cls ?title ?style ?lang ?dir ?charset ?hreflang ~rel:[rel]
    ?rev ~typ:"text/html" href

let start = link_simple "Start"
let next = link_simple "Next"
let prev = link_simple "Prev"
let contents = link_simple "Contents"
let index = link_simple "Index"
let glossary = link_simple "Glossary"
let copyright_link = link_simple "Copyright"
let chapter = link_simple "Chapter"
let section = link_simple "Section"
let subsection = link_simple "Subsection"
let appendix = link_simple "Appendix"
let help = link_simple "Help"
let bookmark = link_simple "Bookmark"

let anchor ?id ?cls ?lang ?dir ?style ?title ?href c =
  html_element_text "a" ?id ?cls ?lang ?dir ?style ?title
    ~attr:(html_attribute ["href", NMTOKEN href] None)
    c

let br = html_element_empty_text "br" ~attr:[]

let strong = html_element_markup "strong"
let em = html_element_markup "em"
let var = html_element_markup "var"
let kbd = html_element_markup "kbd"
let abbr = html_element_markup "abbr"
let acronym = html_element_markup "acronym"
let cite = html_element_markup "cite"
let code = html_element_markup "code"
let samp = html_element_markup "samp"
let dfn = html_element_markup "dfn"


let h1 = html_element_hier "h1"
let h2 = html_element_hier "h2"
let h3 = html_element_hier "h3"
let h4 = html_element_hier "h4"
let h5 = html_element_hier "h5"
let h6 = html_element_hier "h6"

let p = html_element_paragraph "p"
let pre = html_element_block "pre"

let quote (
  f, name :
    (nmtoken ->
       ?id:nmtoken ->
      ?cls:nmtoken ->
      ?lang:nmtoken ->
      ?dir:nmtoken ->
      ?style:cdata ->
      ?title:cdata ->
      ?attr:(nmtoken * cdata) list -> 'a -> snippet
    ) * nmtoken )
    ?id ?cls ?lang ?dir ?style
    ?title ?cite c =
  f name ?id ?cls ?lang ?dir ?style ?title
    ~attr:(
      html_attribute [
	"cite", NMTOKEN cite;
      ] None
    ) c

let q = quote (html_element_text,"q")
let blockquote = quote (html_element_structure, "blockquote")

let div = html_element_block "div"
let span = html_element_markup "span"

let address = html_element_block "address"

let edition name ?id ?cls ?lang ?dir ?style
    ?title ?cite ?date c =
  html_element_structure name
    ~attr:(
      html_attribute [
	"id", NMTOKEN id;
	"class", NMTOKEN cls;
	"title", CDATA title;
	"style", CDATA style;
	"lang", NMTOKEN lang;
	"dir", NMTOKEN dir;
	"cite", NMTOKEN cite;
	"date", NMTOKEN date;
      ] None
    ) [cdata c]

let ins = edition "ins"
let del = edition "del"

let ul = html_element_structure "ul"
let li = html_element_structure "li"


let img ?id ?cls ?lang ?dir ?style ?title ?name ?height ?width ?usemap
    ?ismap ?longdesc ~alt src =
  html_element_empty_text "img" ?id ?cls ?lang ?dir ?style ?title ~attr:(
      html_attribute [
	"name", NMTOKEN name;
	"height", INTEGER height;
	"width", INTEGER width;
	"usemap", NMTOKEN usemap;
	"ismap", NMTOKEN ismap;
	"longdesc", NMTOKEN longdesc;
	"alt", CDATA (Some alt);
	"src", NMTOKEN (Some src);
      ] None
  ) ()

let body = html_element_block "body"
let head ?lang ?dir ~title c =
  element "head" ~empty:false ~block:true ~attr:[]
    ( html_element_block "title" ?lang ?dir [cdata title] :: c)

let html ?(enc = Encoding.locale) c =
  make ~declaration:(
    "<?xml version=\"1.0\" encoding=\"" ^
      (Encoding.name enc) ^
      "\"?>"
  ) ~dtd:"<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01//EN\">"
    [html_element_block "html" c]

let cdata = cdata
let c x = cdata (u x)


let ol = html_element_block "ol"
let ul = html_element_block "ul"
let li = html_element_block "li"
let dl = html_element_block "dl"
let dt = html_element_block "dt"
let dd = html_element_block "dd"
