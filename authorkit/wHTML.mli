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
(** Simple writer for HTML files

We define rudimentary facilities to write HTML files. These facilites are
built atop the WSGML module. *)

open WSGML

val cdata: cdata -> snippet
(** Conversion from character data. *)

val pcdata: pcdata -> snippet
(** Conversion from character data. *)

val c: string -> snippet
(** Conversion from Latin-1 strings. *)

(** {2 META Data} *)

val meta : ?lang:nmtoken -> ?dir:nmtoken  -> ?http_equiv:nmtoken ->
  ?name:nmtoken -> ?scheme:nmtoken -> cdata -> snippet
(** Create an META tag. *)

val author : ?lang:nmtoken -> ?dir:nmtoken -> cdata -> snippet
(** Meta author tag. *)

val generator : ?lang:nmtoken -> ?dir:nmtoken -> cdata -> snippet
(** Meta generator tag. *)

val copyright : ?lang:nmtoken -> ?dir:nmtoken -> cdata -> snippet
(** Meta copyright tag. *)

val keywords : ?lang:nmtoken -> ?dir:nmtoken -> cdata list -> snippet
(** Meta keywords tag. *)

val date : ?lang:nmtoken -> ?dir:nmtoken -> cdata -> snippet
(** Meta date tag. *)

val default_style : ?lang:nmtoken -> ?dir:nmtoken -> cdata -> snippet
(** Meta default style tag. *)


(** {2 LINK Tags} *)

val link : ?id:nmtoken -> ?cls:nmtoken -> ?title:cdata -> ?style:cdata ->
  ?lang:nmtoken -> ?dir:nmtoken -> ?charset:nmtoken -> ?hreflang:nmtoken ->
  ?rel:nmtoken list -> ?rev:nmtoken list -> ?typ:nmtoken -> nmtoken -> snippet
(** [link href] creates a link tag with the given [href] attribute. Optional
parameters give control over other attributes. *)

val css_persistent : ?id:nmtoken -> ?cls:nmtoken -> ?style:cdata ->
  nmtoken -> snippet
(** [css_persistent href] link to persistent CSS. *)

val css_preferred : ?id:nmtoken -> ?cls:nmtoken -> ?style:cdata ->
  title:cdata -> nmtoken -> snippet
(** [css_preferred ~title href] link to the author's preferred CSS. *)

val css_alternate : ?id:nmtoken -> ?cls:nmtoken -> ?style:cdata ->
  title:cdata -> nmtoken -> snippet
(** [css_alternate ~title href] link to the author's preferred CSS. *)

val style : ?lang:nmtoken -> ?dir:nmtoken -> ?typ:nmtoken ->
  ?title:cdata -> cdata -> snippet
(** [style s] insert inline style information. *)

val css_inline_persistent : cdata -> snippet
(** [css_inline_persistent style] link to persistent CSS. *)

val css_inline_preferred : ?lang:nmtoken -> ?dir:nmtoken ->
  title:cdata -> cdata -> snippet
(** [css_inline_preferred ~title style] link to the author's preferred CSS. *)

val start : ?id:nmtoken -> ?cls:nmtoken -> ?title:cdata -> ?style:cdata ->
  ?lang:nmtoken -> ?dir:nmtoken -> ?charset:nmtoken -> ?hreflang:nmtoken ->
  ?rev:nmtoken list -> nmtoken -> snippet
(** Refer to the first document in a collection of documents. This link type
tells search engines which document is considered by the author to be the
starting point of the collection. *)

val next : ?id:nmtoken -> ?cls:nmtoken -> ?title:cdata -> ?style:cdata ->
  ?lang:nmtoken -> ?dir:nmtoken -> ?charset:nmtoken -> ?hreflang:nmtoken ->
  ?rev:nmtoken list -> nmtoken -> snippet
(** Refer to the next document in a linear sequence of documents. User agents
may choose to preload the "next" document, to reduce the perceived load
time. *)

val prev : ?id:nmtoken -> ?cls:nmtoken -> ?title:cdata -> ?style:cdata ->
  ?lang:nmtoken -> ?dir:nmtoken -> ?charset:nmtoken -> ?hreflang:nmtoken ->
  ?rev:nmtoken list -> nmtoken -> snippet
(** Refer to the previous document in an ordered series of documents. Some
user agents also support the synonym "Previous". *)

val contents : ?id:nmtoken -> ?cls:nmtoken -> ?title:cdata -> ?style:cdata ->
  ?lang:nmtoken -> ?dir:nmtoken -> ?charset:nmtoken -> ?hreflang:nmtoken ->
  ?rev:nmtoken list -> nmtoken -> snippet
(** Refer to a document serving as a table of contents. Some user agents also
support the synonym ToC (from "Table of Contents"). *)

val index : ?id:nmtoken -> ?cls:nmtoken -> ?title:cdata -> ?style:cdata ->
  ?lang:nmtoken -> ?dir:nmtoken -> ?charset:nmtoken -> ?hreflang:nmtoken ->
  ?rev:nmtoken list -> nmtoken -> snippet
(** Refer to a document providing an index for the current document. *)

val glossary : ?id:nmtoken -> ?cls:nmtoken -> ?title:cdata -> ?style:cdata ->
  ?lang:nmtoken -> ?dir:nmtoken -> ?charset:nmtoken -> ?hreflang:nmtoken ->
  ?rev:nmtoken list -> nmtoken -> snippet
(** Refer to a document providing a glossary of terms that pertain to the
current document. *)

val copyright_link : ?id:nmtoken -> ?cls:nmtoken -> ?title:cdata -> ?style:cdata ->
  ?lang:nmtoken -> ?dir:nmtoken -> ?charset:nmtoken -> ?hreflang:nmtoken ->
  ?rev:nmtoken list -> nmtoken -> snippet
(** Refer to a copyright statement for the current document. *)

val chapter : ?id:nmtoken -> ?cls:nmtoken -> ?title:cdata -> ?style:cdata ->
  ?lang:nmtoken -> ?dir:nmtoken -> ?charset:nmtoken -> ?hreflang:nmtoken ->
  ?rev:nmtoken list -> nmtoken -> snippet
(** Refer to a document serving as a chapter in a collection of documents. *)

val section : ?id:nmtoken -> ?cls:nmtoken -> ?title:cdata -> ?style:cdata ->
  ?lang:nmtoken -> ?dir:nmtoken -> ?charset:nmtoken -> ?hreflang:nmtoken ->
  ?rev:nmtoken list -> nmtoken -> snippet
(** Refer to a document serving as a section in a collection of documents. *)

val subsection : ?id:nmtoken -> ?cls:nmtoken -> ?title:cdata -> ?style:cdata ->
  ?lang:nmtoken -> ?dir:nmtoken -> ?charset:nmtoken -> ?hreflang:nmtoken ->
  ?rev:nmtoken list -> nmtoken -> snippet
(** Refer to a document serving as a subsection in a collection of
documents. *)

val appendix : ?id:nmtoken -> ?cls:nmtoken -> ?title:cdata -> ?style:cdata ->
  ?lang:nmtoken -> ?dir:nmtoken -> ?charset:nmtoken -> ?hreflang:nmtoken ->
  ?rev:nmtoken list -> nmtoken -> snippet
(** Refer to a document serving as an appendix in a collection of
documents. *)

val help : ?id:nmtoken -> ?cls:nmtoken -> ?title:cdata -> ?style:cdata ->
  ?lang:nmtoken -> ?dir:nmtoken -> ?charset:nmtoken -> ?hreflang:nmtoken ->
  ?rev:nmtoken list -> nmtoken -> snippet
(** Refer to a document offering help (more information, links to other
sources information, etc.) *)

val bookmark : ?id:nmtoken -> ?cls:nmtoken -> ?title:cdata -> ?style:cdata ->
  ?lang:nmtoken -> ?dir:nmtoken -> ?charset:nmtoken -> ?hreflang:nmtoken ->
  ?rev:nmtoken list -> nmtoken -> snippet
(** Refer to a bookmark. A bookmark is a link to a key entry point within an
extended document. The title attribute may be used, for example, to label the
bookmark. Note that several bookmarks may be defined in each document. *)



(** {2 Structured text: Phrase elements} *)

val anchor : ?id:nmtoken -> ?cls:nmtoken -> ?lang:nmtoken -> ?dir:nmtoken ->
  ?style:cdata -> ?title:cdata -> ?href:nmtoken -> cdata -> snippet
(** Anchor. *)

val em : ?id:nmtoken -> ?cls:nmtoken -> ?lang:nmtoken -> ?dir:nmtoken ->
  ?style:cdata -> ?title:cdata -> cdata -> snippet
(** Indicate emphasis. *)

val strong : ?id:nmtoken -> ?cls:nmtoken -> ?lang:nmtoken -> ?dir:nmtoken ->
  ?style:cdata -> ?title:cdata -> cdata -> snippet
(** Indicate stronger emphasis. *)

val cite : ?id:nmtoken -> ?cls:nmtoken -> ?lang:nmtoken -> ?dir:nmtoken ->
  ?style:cdata -> ?title:cdata -> cdata -> snippet
(** Contains a citation or references to other sources. *)

val dfn : ?id:nmtoken -> ?cls:nmtoken -> ?lang:nmtoken -> ?dir:nmtoken ->
  ?style:cdata -> ?title:cdata -> cdata -> snippet
(** Indicate that this is the defining instance of the enclosed term. *)

val code : ?id:nmtoken -> ?cls:nmtoken -> ?lang:nmtoken -> ?dir:nmtoken ->
  ?style:cdata -> ?title:cdata -> cdata -> snippet
(** Designate a fragment of computer code. *)

val samp : ?id:nmtoken -> ?cls:nmtoken -> ?lang:nmtoken -> ?dir:nmtoken ->
  ?style:cdata -> ?title:cdata -> cdata -> snippet
(** Designate sample output from programs, scripts, etc. *)

val kbd : ?id:nmtoken -> ?cls:nmtoken -> ?lang:nmtoken -> ?dir:nmtoken ->
  ?style:cdata -> ?title:cdata -> cdata -> snippet
(** Indicate text to be entered by the user. *)

val var : ?id:nmtoken -> ?cls:nmtoken -> ?lang:nmtoken -> ?dir:nmtoken ->
  ?style:cdata -> ?title:cdata -> cdata -> snippet
(** Indicate an instance of a variable or program argument. *)

val abbr : ?id:nmtoken -> ?cls:nmtoken -> ?lang:nmtoken -> ?dir:nmtoken ->
  ?style:cdata -> ?title:cdata -> cdata -> snippet
(** Indicate an abbreviated form (e.g., WWW, HTTP, URI, Mass., etc.). *)

val acronym : ?id:nmtoken -> ?cls:nmtoken -> ?lang:nmtoken -> ?dir:nmtoken ->
  ?style:cdata -> ?title:cdata -> cdata -> snippet
(** Indicate an acronym (e.g., WAC, radar, etc.). *)



(** {2 Quotations} *)

val blockquote : ?id:nmtoken -> ?cls:nmtoken -> ?lang:nmtoken -> ?dir:nmtoken ->
  ?style:cdata -> ?title:cdata -> ?cite:nmtoken -> snippet list -> snippet
(** A paragraph containing a long quotation. *)

val q : ?id:nmtoken -> ?cls:nmtoken -> ?lang:nmtoken -> ?dir:nmtoken ->
  ?style:cdata -> ?title:cdata -> ?cite:nmtoken -> cdata -> snippet
(** A short quotation. *)



(** {2 Marking text changes} *)

val ins : ?id:nmtoken -> ?cls:nmtoken -> ?lang:nmtoken -> ?dir:nmtoken ->
  ?style:cdata -> ?title:cdata ->  ?cite:nmtoken ->
  ?date:nmtoken -> cdata -> snippet
(** Indicate an insertion. *)

val del : ?id:nmtoken -> ?cls:nmtoken -> ?lang:nmtoken -> ?dir:nmtoken ->
  ?style:cdata -> ?title:cdata -> ?cite:nmtoken ->
  ?date:nmtoken -> cdata -> snippet
(** Indicate a deletion. *)



(** {2 Lines and paragaphs} *)

val p : ?id:nmtoken -> ?cls:nmtoken -> ?lang:nmtoken -> ?dir:nmtoken ->
  ?style:cdata -> ?title:cdata -> snippet list -> snippet

val br : ?id:nmtoken -> ?cls:nmtoken -> ?lang:nmtoken -> ?dir:nmtoken ->
  ?style:cdata -> ?title:cdata -> unit -> snippet

val pre : ?id:nmtoken -> ?cls:nmtoken -> ?lang:nmtoken -> ?dir:nmtoken ->
  ?style:cdata -> ?title:cdata -> snippet list -> snippet


(** {2 Grouping elements} *)

val span : ?id:nmtoken -> ?cls:nmtoken -> ?lang:nmtoken -> ?dir:nmtoken ->
  ?style:cdata -> ?title:cdata -> cdata -> snippet
(** Mark inline content. *)

val div : ?id:nmtoken -> ?cls:nmtoken -> ?lang:nmtoken -> ?dir:nmtoken ->
  ?style:cdata -> ?title:cdata -> snippet list -> snippet
(** Mark block-level content. *)



(** {2 Headings} *)

val h1 : ?id:nmtoken -> ?cls:nmtoken -> ?lang:nmtoken -> ?dir:nmtoken ->
  ?style:cdata -> ?title:cdata -> cdata -> snippet

val h2 : ?id:nmtoken -> ?cls:nmtoken -> ?lang:nmtoken -> ?dir:nmtoken ->
  ?style:cdata -> ?title:cdata -> cdata -> snippet

val h3 : ?id:nmtoken -> ?cls:nmtoken -> ?lang:nmtoken -> ?dir:nmtoken ->
  ?style:cdata -> ?title:cdata -> cdata -> snippet

val h4 : ?id:nmtoken -> ?cls:nmtoken -> ?lang:nmtoken -> ?dir:nmtoken ->
  ?style:cdata -> ?title:cdata -> cdata -> snippet

val h5 : ?id:nmtoken -> ?cls:nmtoken -> ?lang:nmtoken -> ?dir:nmtoken ->
  ?style:cdata -> ?title:cdata -> cdata -> snippet

val h6 : ?id:nmtoken -> ?cls:nmtoken -> ?lang:nmtoken -> ?dir:nmtoken ->
  ?style:cdata -> ?title:cdata -> cdata -> snippet


(** {2 The address element} *)

val address : ?id:nmtoken -> ?cls:nmtoken -> ?lang:nmtoken -> ?dir:nmtoken ->
  ?style:cdata -> ?title:cdata -> snippet list -> snippet
(** The ADDRESS element may be used by authors to supply contact information
for a document or a major part of a document such as a form. This element
often appears at the beginning or end of a document. *)


(** {2 The image element}

Nota: maps are not yet supported. *)

val img : ?id:nmtoken -> ?cls:nmtoken -> ?lang:nmtoken -> ?dir:nmtoken ->
  ?style:cdata -> ?title:cdata -> ?name:nmtoken -> ?height:int ->
  ?width:int -> ?usemap:nmtoken -> ?ismap:nmtoken -> ?longdesc:nmtoken ->
  alt:cdata -> nmtoken -> snippet
(** The IMG element. The alternate description is mandatory,
internationalisation attributes refer to it. *)


(** {2 Lists} *)

val ol: ?id:nmtoken -> ?cls:nmtoken -> ?lang:nmtoken -> ?dir:nmtoken ->
  ?style:cdata -> ?title:cdata -> snippet list -> snippet
(** Ordered lists. *)

val ul: ?id:nmtoken -> ?cls:nmtoken -> ?lang:nmtoken -> ?dir:nmtoken ->
  ?style:cdata -> ?title:cdata -> snippet list -> snippet
(** Unordered lists. *)

val li: ?id:nmtoken -> ?cls:nmtoken -> ?lang:nmtoken -> ?dir:nmtoken ->
  ?style:cdata -> ?title:cdata -> snippet list -> snippet
(** List items. *)

val dl: ?id:nmtoken -> ?cls:nmtoken -> ?lang:nmtoken -> ?dir:nmtoken ->
  ?style:cdata -> ?title:cdata -> snippet list -> snippet
(** Definitions lists. *)

val dt: ?id:nmtoken -> ?cls:nmtoken -> ?lang:nmtoken -> ?dir:nmtoken ->
  ?style:cdata -> ?title:cdata -> snippet list -> snippet
(** Definition tag. *)

val dd: ?id:nmtoken -> ?cls:nmtoken -> ?lang:nmtoken -> ?dir:nmtoken ->
  ?style:cdata -> ?title:cdata -> snippet list -> snippet
(** Defintion description. *)


(** {2 The document} *)

val body : ?id:nmtoken -> ?cls:nmtoken -> ?lang:nmtoken -> ?dir:nmtoken ->
  ?style:cdata -> ?title:cdata -> snippet list -> snippet
(** Body of the document. *)

val head: ?lang:nmtoken -> ?dir:nmtoken -> title:cdata -> snippet list ->
  snippet
(** The head of a document. *)

val html: ?enc:Unicode.Encoding.t -> snippet list -> t
(** The full document. *)
