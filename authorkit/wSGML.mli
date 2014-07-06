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
(** Simple writer for SGML files

We define rudimentary facilities to assemble SGML files. These facilities are
organised under the notion of {i snippet}. A snippet represents a
well-balanced fragment of a hypothetical SGML document (the document being
assembled). Once they are created, snippets can be pasted together to build
larger snippets. Ultimately, the final snippet is rendered in a buffer,
a string, or output on a channel stream. *)


(** {2 Text occuring in snippets}

The text occuring in document snippets comes in several flavours. All of these
are unicode strings, but they differ in the way they are used. *)

type cdata = Unicode.ustring
(** The type of {i character data}. This kind of data is pasted into the
output without further processing. This type is typically used for giving
titles, identifiers, etc. to elements. *)

type pcdata = Unicode.ustring
(** The type of {i parsed character data}. This kind of data is further
processed before pasting, in particulat SGML reserved characters are
escaped. *)

type nmtoken = string
(** The type of {i name tokens}. This kind of data is used to labele elements
and keys of their attributes. *)

val escape: pcdata -> cdata
(** [escape s] returns an escaped form for [s], ready to be pasted in the
final document .*)

type snippet
(** The type of {i snippets}. A snippet can be a piece of text or an element,
as described below. *)

val cdata : cdata -> snippet
(** Create a simple snippet containing free character data. *)

val pcdata: pcdata -> snippet
(** Create a simple snippet containing parsed character data. *)

type element
(** The type of {i elements}. Elements have a name, a list of attributes and a
content. The empty field tells the renderer wether the closing tag may be
omitted and the block field is a rendering hint distinguishing block level
elements of text levels. Note that this vision of SGML elements is rather
approximative!  *)

val element : nmtoken -> ?empty:bool -> ?block:bool ->
  ?attr:(nmtoken * cdata) list -> snippet list -> snippet
(** [element name content] create a complex snippet, consisting of an element
named [name] containing the snippets specified in [content].

@raise Invalid_arg if [name] is the empty string. *)

(** {2 Pasting snippets} *)

(** The formatting functions below that require an encoding will fail
if the given encoding is not able to represent all the characters
found in the document being formatted. *)

type t
(** The type of {i final documents}. *)

val make : ?declaration:nmtoken -> ?dtd:nmtoken -> snippet list -> t
(** [make content] create a document snippet with the specified
content. It is possible to preprend a declaration and a
DTD identifier to the resulting document. *)

val uformat : Unicode.UFormat.formatter -> t -> unit
(** [format f d] format document [d] to unicode formatter [f]. *)

val format : ?enc:Unicode.Encoding.t -> Format.formatter -> t -> unit
(** [format f d] format document [d] to formatter [f]. *)

val add_to_buffer : ?enc:Unicode.Encoding.t -> Buffer.t -> t -> unit
(** Format a document to an existing unicode buffer. *)

val to_buffer : ?enc:Unicode.Encoding.t -> t -> Buffer.t
(** Format a document to a fresh new buffer. *)

val to_string : ?enc:Unicode.Encoding.t -> t -> string
(** Format a document to a fresh new string. *)

val output : ?enc:Unicode.Encoding.t -> out_channel -> t -> unit
(** Write the document to the output channel. *)

val print : ?enc:Unicode.Encoding.t -> t -> unit
(** Write the document to the standard output. This uses the Unicode's module
presentation of standard output, so encoding can be set using
[Unicode.Uchannel] module. *)

val write : ?enc:Unicode.Encoding.t -> string -> t -> unit
(** Turn the document into an encoded file. This may fail if the required
encoding does not support all of the characters occuring in the document. *)

(** {2 Factory}

We provide a few services in order to simplify the elaboration of complex
documents. *)

val concat: snippet list list -> snippet list
(** This is an avatar of [List.concat]. It is useful when preparing the
content of an element. *)

val outcome: (('a -> 'b) * ('a option)) list -> 'b list
(** [outcome [(f1, opt1); ...]] return the list deduced from its argument by:
filtering out pairs having [None] as second member, and replacing [(f,Some v)]
by [f v].

This procedure may be useful when writing a function with a big number of
optional arguments, describing optional parts of a document. *)
