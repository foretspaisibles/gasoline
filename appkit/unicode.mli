(* Unicode -- Unicode facility

Author: Michael Grünewald
Date: Sun Nov  9 06:52:49 CET 2008

Gasoline (https://github.com/michipili/gasoline)
This file is part of Gasoline

Copyright © 2008–2014 Michael Grünewald

This file must be used under the terms of the CeCILL-B.
This source file is licensed as described in the file COPYING, which
you should have received as part of this distribution. The terms
are also available at
http://www.cecill.info/licences/Licence_CeCILL-B_V1-en.txt *)
(** Unicode facility.

Camomile is a complete Unicode library, by Yamagata Yoriyuki. This {i Unicode}
module is a simple wrapper around some {i Camomile} modules.

The recommanded use for this module is to be {i opened} with the [open
Unicode] statement. *)

type uchar
(** The type of unicode chars. *)

type ustring
(** The type of unicode strings. *)


(** Sets of Unicode characters

These sets are implemented as sets of intervals.  The signature is
mostly same to Set.S in {i stdlib}. *)
module USet :
sig

type t
type elt = uchar

val empty : t
val is_empty : t -> bool
val mem : uchar -> t -> bool
val add : uchar -> t -> t

val add_range : uchar -> uchar -> t -> t
(** [add_range u1 u2 s] adds the characters in the range from
    [u1 - u2] to [s]. The range is determined by the code point order. *)

val singleton : uchar -> t
(** [singleton u] returns the one-element set containing only [u]. *)

val remove : uchar -> t -> t
(** [remove u s] returns a set containing all elements of [s], except
[u]. If [u] was not in [s], [s] is returned unchanged. *)

val remove_range : uchar -> uchar -> t -> t
(** [remove_range u1 u2 s] removes the characters in the range [u1] - [u2]
from [s].  The range is determined by the code point order. *)

val union : t -> t -> t
(** Set union. *)

val inter : t -> t -> t
(** Set intersection. *)

val diff : t -> t -> t
(** [diff s1 s2] is the set whose elements are those which belongs to
only one of the sets [s1] and [s2]. *)

val compl : t -> t
(** [compl s] is the compliment of [s]. *)

val compare : t -> t -> int
(** Total ordering between sets. Can be used as the ordering function
for doing, e.g., sets of sets. *)

val equal : t -> t -> bool
(** [equal s1 s2] tests whether the sets [s1] and [s2] are equal, that
is, contain equal elements. *)

val subset : t -> t -> bool
(** [subset s1 s2] is true iff [s1] is a subset of [s2]. *)

val from : uchar -> t -> t
(** [from u s] returns the set of elements of [s]
whose code points are equal or greater than [u]. *)

val after : uchar -> t -> t
(** [after u s] returns the set of elements of [s]
whose code points are greater than [u]. *)

val until : uchar -> t -> t
(** [until u s] returns the set of elements of [s]
whose code points are equal or smaller than [u]. *)

val before : uchar -> t -> t
(** [until u s] returns the set of elements of [s]
whose code points are smaller than [u]. *)

val iter : (uchar -> unit) -> t -> unit
(** Iter a function over the elements of a set. *)

val iter_range : (uchar -> uchar -> unit) -> t -> unit
(** [iter_range proc s] feeds the intervals contained in [s] to
[proc] in increasing order.  The intervals given to [proc]
are always separated by the character not in [s]. *)

val fold : (uchar -> 'a -> 'a) -> t -> 'a -> 'a
(** Fold over elements of a set. *)

val fold_range : (uchar -> uchar -> 'a -> 'a) -> t -> 'a -> 'a
(** [fold_range f s x] is equivalent to
[f u_i u_(i+1) (... (f u_3 u_4 (f u_1 u_2 x)))] if [s] is consisted of
the intervals [u1]-[u2], [u3]-[u4], ..., [u_i]-[u_(i + 1)]
in increasing order.  The intervals given to [proc]
are always separated by the character not in [s]. *)

val for_all : (uchar -> bool) -> t -> bool
(** [for_all p s] checks if all elements of the set satisfy the
predicate [p]. *)

val exists : (uchar -> bool) -> t -> bool
(** [exists p s] checks if at least one element of the set satisfies
the predicate [p]. *)

val filter : (uchar -> bool) -> t -> t
(** [filter p s] returns the set of all elements in [s] that satisfy
predicate [p]. *)

val partition : (uchar -> bool) -> t -> t * t
(** [partition p s] returns a pair of sets [(s1, s2)], where [s1] is
the set of all the elements of [s] that satisfy the predicate [p], and
[s2] is the set of all the elements of [s] that do not satisfy [p]. *)

val cardinal : t -> int
(** Return the number of elements of a set. *)

val elements : t -> uchar list
(** Return the list of all elements of the given set.
The returned list is sorted in increasing order with respect
to the ordering [UChar.compare]. *)

val ranges : t -> (uchar * uchar) list
(** The list of the intervals contained in the set.
The returned intervals are always separated
by the character not in [s]. *)

val min_elt : t -> uchar
(** Return the smallest element of the given set
(with respect to the [UChar.compare] ordering), or raise
[Not_found] if the set is empty. *)

val max_elt : t -> uchar
(** Same as {!USet.min_elt}, but returns the largest element of the
given set. *)

val choose : t -> uchar
(** Returns a element roughly in the middle of the set.
It is not guaranteed to return the same element for
the sets with the same elements *)

val split: uchar -> t -> t * bool * t
(** [split u s] returns a triple [(l, present, r)], where [l] is the
set of elements of [s] that are strictly less than [u]; [r] is the set
of elements of [s] that are strictly greater than [u]; [present] is
[false] if [s] contains no element equal to [u], or [true] if [s]
contains an element equal to [u]. *)
end


(** Maps over Unicode characters *)
module UMap :
sig

type key = uchar
(** The type of the map keys. *)


type 'a t
(** The type of maps from type [uchar] to type ['a]. *)

val empty : 'a t
(** The empty map. *)

val is_empty : 'a t -> bool
(** Test whether a map is empty or not. *)

val add : uchar -> 'a -> 'a t -> 'a t
(** [add u v m] returns a map containing the same bindings as [m],
plus a binding of [u] to [u]. If [u] was already bound in [m], its
previous binding disappears. *)

val add_range : uchar -> uchar -> 'a -> 'a t -> 'a t
(** [add u1 u2 v m] returns the new map which is same to [m] except it
maps characters in the range [u1 - u2] to [v]. *)

val find : uchar -> 'a t -> 'a
(** [find u m] returns the current binding of [u] in [m], or raises
[Not_found] if no such binding exists. *)

val remove : uchar -> 'a t -> 'a t
(** [remove u m] returns a map containing the same bindings as [m],
except for [u] which is unbound in the returned map. *)

val remove_range : uchar -> uchar -> 'a t -> 'a t
(** [remove_range u1 u2 m] removes [u1 - u2] from the domain of [m] *)

val from : uchar -> 'a t -> 'a t
(** [from u m] restricts the domain of [m] to the characters whose
code points are equal or greater than [u]. *)

val after : uchar -> 'a t -> 'a t
(** [after u m] restricts the domain of [m] to the characters whose
code points are greater than [u]. *)

val until : uchar -> 'a t -> 'a t
(** [until u m] restricts the domain of [m] to the characters whose
code points are equal or smaller than [u]. *)

val before : uchar -> 'a t -> 'a t
(** [before u m] restricts the domain of [m] to the characters whose
code points are smaller than [u]. *)

val mem : uchar -> 'a t -> bool
(** [mem u m] returns [true] if [m] contains a binding for [u],
and [false] otherwise. *)

val iter : (uchar -> 'a -> unit) -> 'a t -> unit
(** [iter f m] applies [f] to all bindings in map [m].  [f] receives
the key as first argument, and the associated value as second
argument.  The bindings are passed to [f] in increasing order with
respect to the ordering over the type of the keys.  Only current
bindings are presented to [f]: bindings hidden by more recent bindings
are not passed to [f]. *)


(** [iter proc m] : For each contingent region [u1]-[u2]
that is mapped to a constant [v], [proc u1 u2 v] is called.
The order of call is determined by increasing order on [u1]. *)
val iter_range : (uchar -> uchar -> 'a -> unit) -> 'a t -> unit

(** [map f m] returns a map with same domain as [m], where the
associated value [a] of all bindings of [m] has been replaced by the
result of the application of [f] to [a].  The bindings are passed to
[f] in increasing order with respect to the ordering over the type of
the keys. *)
val map : ('a -> 'b) -> 'a t -> 'b t

val mapi : (uchar -> 'a -> 'b) -> 'a t -> 'b t
(** Same as {!UMap.S.map}, but the function receives as arguments both the
key and the associated value for each binding of the map. *)

val fold : (uchar -> 'b -> 'a -> 'a) -> 'b t -> 'a -> 'a
(** [fold f m a] computes [(f kN dN ... (f k1 d1 a)...)],
where [k1 ... kN] are the keys of all bindings in [m]
(in increasing order), and [d1 ... dN] are the associated data. *)

val fold_range : (uchar -> uchar -> 'b -> 'a -> 'a) -> 'b t -> 'a -> 'a
(** [fold_range f m x] is equivalent to [f u_(2n) u_(2n+1) v_n (... (f
u_1 u_2 v_1 x))] where all characters in the range [u_(2k)]-[u_(2k+1)]
are mapped to [v_k] and [u_1] < [u_3] < ... in code point order.  For
each range [u_(2k)]-[u_(2k+1)] is separated by a character which is
not mapped to [v_k]. *)

val constant : USet.t -> 'a -> 'a t
(** Constant map.*)

val domain : 'a t -> USet.t
(** Domain. *)

val subdomain : ('a -> bool) -> 'a t -> USet.t
(** [subdomain p m] returns the set of characters which are mapped
to values satisfying the predicate [p] by [m]. *)

val compare: ('a -> 'a -> int) -> 'a t -> 'a t -> int
(** Total ordering between maps.  The first argument is a total
ordering used to compare data associated with equal keys in the two
maps. *)

val equal: ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
(** [equal cmp m1 m2] tests whether the maps [m1] and [m2] are equal,
that is, contain equal keys and associate them with equal data.  [cmp]
is the equality predicate used to compare the data associated with the
keys. *)

end


(** Lookup tables with keys over Unicode characters *)
module UTable :
sig
  type 'a t
  val get: 'a t -> uchar -> 'a
end

module type UTABLE =
sig
  type t
  type elt
  val get: t -> uchar -> elt
end

module UTable_bool : UTABLE with type elt = bool
module UTable_bits : UTABLE with type elt = int
module UTable_bytes : UTABLE with type elt = int
module UTable_char : UTABLE with type elt = char

(** Character Information *)
module UInformation:
sig

(** The type of Unicode primary character categories. *)
type primary_category =
  | LETTER
  | MARK
  | NUMBER
  | SEPARATOR
  | OTHER
  | PUNCTUATION
  | SYMBOL

(** The type of Unicode general character categories. *)
type general_category =
  | LETTER_UPPERCASE
  | LETTER_LOWERCASE
  | LETTER_TITLECASE
  | LETTER_MODIFIER
  | LETTER_OTHER
  | MARK_NON_SPACING
  | MARK_SPACING_COMBINING
  | MARK_ENCLOSING
  | NUMBER_DECIMAL_DIGIT
  | NUMBER_LETTER
  | NUMBER_OTHER
  | SEPARATOR_SPACE
  | SEPARATOR_LINE
  | SEPARATOR_PARAGRAPH
  | OTHER_CONTROL
  | OTHER_FORMAT
  | OTHER_SURROGATE
  | OTHER_PRIVATE_USE
  | OTHER_NOT_ASSIGNED
  | PUNCTUATION_CONNECTOR
  | PUNCTUATION_DASH
  | PUNCTUATION_OPEN
  | PUNCTUATION_CLOSE
  | PUNCTUATION_INITIAL
  | PUNCTUATION_FINAL
  | PUNCTUATION_OTHER
  | SYMBOL_MATH
  | SYMBOL_CURRENCY
  | SYMBOL_MODIFIER
  | SYMBOL_OTHER

(** The type of Unicode character properties. *)
type character_property =
  (** Derived Core Properties*)
  | MATH
  | ALPHABETIC
  | LOWERCASE
  | UPPERCASE
  | ID_START
  | ID_CONTINUE
  | XID_START
  | XID_CONTINUE
  | DEFAULT_IGNORABLE_CODE_POINT
  | GRAPHEME_EXTEND
  | GRAPHEME_BASE
  (**Extended Properties*)
  | BIDI_CONTROL
  | WHITE_SPACE
  | HYPHEN
  | QUOTATION_MARK
  | TERMINAL_PUNCTUATION
  | OTHER_MATH
  | HEX_DIGIT
  | ASCII_HEX_DIGIT
  | OTHER_ALPHABETIC
  | IDEOGRAPHIC
  | DIACRITIC
  | EXTENDER
  | OTHER_LOWERCASE
  | OTHER_UPPERCASE
  | NONCHARACTER_CODE_POINT
  | GRAPHEME_LINK
  | OTHER_GRAPHEME_EXTEND
  | IDS_BINARY_OPERATOR
  | IDS_TRINARY_OPERATOR
  | RADICAL
  | UNIFIED_IDEOGRAPH
  | OTHER_DEFAULT_IGNORABLE_CODE_POINT
  | DEPRECATED
  | SOFT_DOTTED
  | LOGICAL_ORDER_EXCEPTION


(** The type of Unicode scripts. *)
type script =
  | ARABIC
  | ARMENIAN
  | BENGALI
  | BOPOMOFO
  | BUHID
  | CANADIAN_ABORIGINAL
  | CHEROKEE
  | COMMON
  | CYRILLIC
  | DESERET
  | DEVANAGARI
  | ETHIOPIC
  | GEORGIAN
  | GOTHIC
  | GREEK
  | GUJARATI
  | GURMUKHI
  | HAN
  | HANGUL
  | HANUNOO
  | HEBREW
  | HIRAGANA
  | INHERITED
  | KANNADA
  | KATAKANA
  | KHMER
  | LAO
  | LATIN
  | MALAYALAM
  | MONGOLIAN
  | MYANMAR
  | OGHAM
  | OLD_ITALIC
  | ORIYA
  | RUNIC
  | SINHALA
  | SYRIAC
  | TAGALOG
  | TAGBANWA
  | TAMIL
  | TELUGU
  | THAANA
  | THAI
  | TIBETAN
  | YI


(** The type of Unicode casemap conditions. *)
type casemap_condition =
  | AFTER_SOFT_DOTTED
  | BEFORE_DOT
  | FINAL_SIGMA
  | LOCALE of string
  | MORE_ABOVE
  | NOT of casemap_condition


(** The type of Unicode character decomposition. *)
type decomposition =
  | CANON
  | CIRCLE
  | COMPAT
  | FINAL
  | FONT
  | FRACTION
  | INITIAL
  | ISOLATED
  | MEDIAL
  | NARROW
  | NOBREAK
  | SMALL
  | SQUARE
  | SUB
  | SUPER
  | VERTICAL
  | WIDE

(** The type of Unicode character decomposition information. *)
type decomposition_info =
  | CANON_FORM
  | COMPOSITE of decomposition * uchar list
  | HANGUL_SYLLABLE


val general_category : uchar -> general_category
(** General classification of characters. *)

val primary_category : uchar -> primary_category
(** Primary classification of characters. *)

val load_general_category_map : unit -> general_category UMap.t
(** Load the map from characters to their categories. *)

val load_property_table : character_property -> UTable_bool.t
(** Load the table for the given character type. *)

val load_property_set : character_property -> USet.t
(** Load the set of characters of the given character type. *)

val load_property_table_by_name : string -> UTable_bool.t
(** Load the table for the given name of the character type. The name
can be obtained by capitalizing the name of the tag. For instance, the
tag [QUOTATION_MARK] is mapped to the string ["Quotation_Mark"]. *)

val load_property_set_by_name : string -> USet.t
(** Load the set of characters of the given name of the character
    type.  The name can be obtained by capitalizing the name of the
    tag. For instance, the tag [QUOTATION_MARK] is mapped to the string
    ["Quotation_Mark"]. *)

val script : uchar -> script

val load_script_map : unit -> script UMap.t

val lower_table : uchar UTable.t
val upper_table : uchar UTable.t
val title_table : uchar UTable.t

val to_lower: uchar -> uchar
val to_upper: uchar -> uchar
val to_title: uchar -> uchar

(** {2 Classification predicates}

    This is a simplified interface to [primary_category] and
    [general_category]. *)

val is_letter : uchar -> bool
(** The [is_letter] functions tests for a letter, that is, a character
    whose primary category is [LETTER]. *)

val is_alpha : uchar -> bool
(** The [is_alpha] function tests for an alphabetic character. It is
    the same as [is_letter]. *)

val is_number : uchar -> bool
(** The [is_number] function tests for a digit character, that is, a
    character whose primary category is [NUMBER]. Note that this
    definition do not match the definition of the standard [isnumber]
    function that matches only decimal digits. *)

val is_mark : uchar -> bool
(** The [is_mark] functions tests for a mark, that is, a character
    whose primary category is [MARK]. *)

val is_separator : uchar -> bool
(** The [is_separator] functions tests for a separator, that is, a character
    whose primary category is [SEPARATOR]. *)

val is_other : uchar -> bool
(** The [is_other] functions tests for a other, that is, a character
    whose primary category is [OTHER]. *)

val is_symbol : uchar -> bool
(** The [is_symbol] functions tests for a symbol, that is, a character
    whose primary category is [SYMBOL]. *)

val is_digit : uchar -> bool
(** The [is_digit] function tests for a decimal digit. *)

val is_alnum : uchar -> bool
(** The [is_alnum] function tests for an alphanumeric character, that
    is, either a letter or a number. *)

val is_ascii : uchar -> bool
(** The [is_ascii] function tests for an ASCII character. *)

val is_latin : uchar -> bool
(** The [is_latin] function tests for a latin character. *)

val is_cntrl : uchar -> bool
(** The [is_cntrl] function tests for a control character. *)

val is_graph : uchar -> bool
(** The [is_graph] function tests for a printing character. White
    space is not considered as a print character. *)

val is_lower : uchar -> bool
(** The [is_lower] function tests for a lowercase character. *)

val is_upper : uchar -> bool
(** The [is_upper] function tests for an uppercase character. *)

val is_title : uchar -> bool
(** The [is_title] function tests for an titlecase character. *)

val is_print : uchar -> bool
(** The [is_print] function tests for a printing character. White
    space is considered as a printing character. *)

val is_punctuation : uchar -> bool
(** The [is_punctuation] function tests for a punctuation character. *)

val is_space : uchar -> bool
(** The [is_space] function tests for a white-space character. These
    white space characters include, but do not limit to, ``\t'' ``\n''
    ``\v'' ``\f'' ``\r'' `` ''. *)

val is_xdigit : uchar -> bool
(** The [is_xdigit] function tests for a hexadecimal digit. *)

val is_odigit : uchar -> bool
(** The [is_odigit] function tests for an octal digit. *)

val is_bdigit : uchar -> bool
(** The [is_bdigit] function tests for a binary digit, that is, either
    ``0'' or ``1''. *)

val is_newline : uchar -> bool
(** The [is_newline] function tests for a newline character. *)

end

(** Codage and transcodage.

See also the [UChannel] module, that contains procedure to adapt char
channels to uchar channels. *)
module Encoding :
sig

exception Malformed_code		(** Failure of decoding. *)
exception Out_of_range			(** Failure of encoding. *)

type t
(** Type for encodings. *)

val automatic : string -> t list -> t -> t
(** [automatic name [enc_1; enc_2; ... enc_n] enc] creates the new
encoding [name] doing automatic decoding detection among [enc_1],
[enc_2], ..., [enc_n] by the given order.  [enc] is used for
encoding. *)

val register : string -> t -> unit
(** [register name enc] register the new encoding [enc] under the name
[name] *)

val alias : string -> string -> unit
(** [alias alias name] : Define [alias] as an alias of the encoding
with the name [name]. *)

val find : string -> t
(** Returns the encoding of the given name.

@raise Not_found if the encoding is unknown.

Encoding names are the same to codeset names in charmap files for the
encodings defined by charmap.  See charmaps directory in the source
directory for the available encodings.  In addition to the encodings
via the charmap files, camomile supports ISO-2022-CN, ISO-2022-JP,
ISO-2022-JP-2, ISO-2022-KR, jauto (Auto detection of Japanese
encodings), UTF-8, UTF-16, UTF-16BE, UTF-16LE.  UTF-32, UTF-32BE,
UTF-32LE, UCS-4(Big endian order). The encoding also can be referred
by "IANA/<IANA name>", if the encoding is supported. *)

val name : t -> string
(** Returns the name of the encoding. *)

(** Some encodings *)
val ascii : t
val latin1 : t
val latin9 : t
val utf8 : t
val utf16 : t
val utf16be : t
val utf16le : t
val utf32 : t
val utf32be : t
val utf32le : t
val ucs4 : t

val locale : t
(** The [locale] is initialized according to the LANG environment
    variable, and defaults to [latin9] if the LANG variable is unset or
    set with an unproper value. *)

val recode_string : in_enc:t -> out_enc:t -> string -> string
(** [recode_string ~in_enc ~out_enc s] converts the string [s] from
    [in_enc] to [out_enc]. *)

val decode : t -> string -> ustring
val encode : t -> ustring -> string

val import: string -> ustring
val export: ustring -> string
(** This is the same duo as decode/encode, using locale to read the data. *)

end


(** Operations on Unicode characters *)
module UChar :
sig
  exception Out_of_range

  (** Unicode characters. All 31 bits code points are allowed.*)
  type t = uchar

  val to_char : t -> char
  (** [to_char u] returns the Latin-9 representation of [u].
  @raise Out_of_range if [u] can not be represented by Latin-9. *)

  val of_char : char -> t
  (** [of_char c] returns the Unicode character of the Latin-1 character [c]. *)

  val code : t -> int
  (** [code u] returns the Unicode code number of [u].
  @raise Out_of_range if [u] cannot be represented by a positive integer. *)

  val chr : int -> t
  (** [chr n] returns the Unicode character with the code number [n].
  @raise Invalid_arg if n >= 2^32 or n < 0. *)

  val uint_code : t -> int
  (** [uint_code u] returns the Unicode code number of [u].
  The returned int is unsigned, that is, on 32-bits platforms,
  the sign bit is used for storing the 31-th bit of the code number. *)

  val uint_chr: int -> t
  (** [uint_chr n] returns the Unicode character of the code number [n].
  [n] is interpreted as unsigned, that is, on 32-bits platforms,
  the sign bit is treated as the 31-th bit of the code number.
  @raise Invalid_arg if [n] exceed 31-bits values. *)

  val eq : t -> t -> bool
  (** Equality by code point comparison *)

  val compare : t -> t -> int
  (** [compare u1 u2] returns,
     a value > 0 if [u1] has a larger Unicode code number than [u2],
     0 if [u1] and [u2] are the same Unicode character,
     a value < 0 if [u1] has a smaller Unicode code number than [u2]. *)

  val to_int : t -> int
  (** Alias of [uint_code] *)

  val of_int : int -> uchar
  (** Alias of [uint_chr] *)

  val newline: uchar
  (** The newline character. *)

  val space : uchar
  (** The space character. *)

  val htab: uchar
  (** The horizontal tab character. *)

  val vtab: uchar
  (** The vertical tab character. *)

  val nextline: uchar
  (** The nextline character. *)

  val line: uchar
  (** The line separator character. *)

  val par: uchar
  (** The paragraph separator character. *)


  (** {2 Output on standard structures} *)

  val format : Encoding.t -> Format.formatter -> uchar -> unit

  val output : Encoding.t -> Pervasives.out_channel -> uchar -> unit

  val print : Encoding.t -> uchar -> unit

  val prerr : Encoding.t -> uchar -> unit

  val printer : Format.formatter -> uchar -> unit
  (** A printer for the toplevel. *)
end


(** Unicode strings *)
module UString:
sig

  type t = ustring
  type index

  val get :  ustring -> int -> uchar

  val set :  ustring -> int -> uchar -> unit
  (** [set s i u] sets the [i]-th character in [s] to [u]. *)

  val look : ustring -> index -> uchar
  val nth : ustring -> int -> index
  val first : ustring -> index
  val last : ustring -> index
  val out_of_range : ustring -> index -> bool
  val compare_index : ustring -> index -> index -> int
  val next : ustring -> index -> index
  val prev : ustring -> index -> index
  val move : ustring -> index -> int -> index
  val length : ustring -> int

  val of_string : string -> ustring
  (** Conversion from Latin-1 strings. *)

  val of_char: char -> ustring
  (** Conversion from Latin-1 chars. *)

  val of_uchar: uchar -> ustring
  (** Conversion from Unicode chars. *)

  val init : int -> (int -> uchar) -> ustring


  (** The semantics of the following functions are similar to their
  equivalents in the standard String module. *)

  val make : int -> uchar -> ustring
  val copy : ustring -> ustring
  val sub : ustring -> int -> int -> ustring
  val fill : ustring -> int -> int -> uchar -> unit
  val blit : ustring -> int -> ustring -> int -> int -> unit
  val append : ustring -> ustring -> ustring
  val iter : (uchar -> unit) -> ustring -> unit
  val compare : ustring -> ustring -> int
  val equal : ustring -> ustring -> bool

  val concat : ustring list -> ustring
  (** Concatenate a list of unicode strings. The list should be reather small,
  if you need to concatenate huge lists, see [UBuffer.add_ustring]. *)

  val join: ustring -> ustring list -> ustring
  (** [join sep l] contatenate the list [l] of unicode string, putting [sep]
  in-between. The list should be reather small, if you need to concatenate
  huge lists, see [UBuffer.join]. *)

  val lowercase : ustring -> ustring
  val uppercase : ustring -> ustring
  val titlecase : ustring -> ustring

  val capitalize: ustring -> ustring
  val uncapitalize : ustring -> ustring

  (** {2 Output on standard structures} *)

  val format : Encoding.t -> Format.formatter -> ustring -> unit

  val output : Encoding.t -> Pervasives.out_channel -> ustring -> unit

  val print : Encoding.t -> ustring -> unit

  val prerr : Encoding.t -> ustring -> unit

  val printer : Format.formatter -> ustring -> unit
  (** A printer for the toplevel. *)

end



(** Unicode I/O channels *)
module UChannel :
sig

  (** {2 General operations} *)

  type in_channel
  (** The type of channels for reading unicode characters. *)

  type out_channel
  (** The type of channels for writing unicode characters. *)

  (** The type encapsulating methods of an output channel. *)
  class type block_out_channel =
  object
    method close_out : unit -> unit
    method flush : unit -> unit
    method output : string -> int -> int -> int
  end

  (** The type encapsulating methods of an input channel. *)
  class type block_in_channel =
  object
    method close_in : unit -> unit
    method input : string -> int -> int -> int
  end


  val from_block_in_channel: ?enc:Encoding.t -> block_in_channel -> in_channel
  (** Create a channel for reading unicode chars from a
  [block_in_channel].

  @param enc is the encoding of the input channel [Encoding.locale].
  *)


  val from_block_out_channel: ?enc:Encoding.t -> block_out_channel -> out_channel
  (** Create a channel for writing chars to a [block_out_channel].

  @param enc is the encoding of the output channel [Encoding.locale].
  *)


  val from_in_channel: ?enc:Encoding.t -> Pervasives.in_channel -> in_channel
  (** Create a channel for reading unicode chars from a
  [Pervasives.in_channel].

  @param enc is the encoding of the input channel [Encoding.locale].
  *)


  val from_out_channel: ?enc:Encoding.t -> Pervasives.out_channel -> out_channel
  (** Create a channel for writing chars to a [Pervasives.out_channel].

  @param enc is the encoding of the output channel [Encoding.locale].
  *)


  val from_formatter:  ?enc:Encoding.t -> Format.formatter -> out_channel
  (** Create a channel for writing chars to a [Format.formatter].

  @param enc is the encoding of the output channel [Encoding.locale].
  *)


  val open_out : ?enc:Encoding.t -> string -> out_channel
  (** Open the named file for writing, and return a new output channel on that
  file, positionned at the beginning of the file. The file is truncated to
  zero length if it already exists. It is created if it does not already
  exists.

  @raise Sys_error if the file could not be opened.
  @param enc is the encoding of the output channel [Encoding.locale].
  *)


  val open_out_bin : ?enc:Encoding.t -> string -> out_channel
  (** Same as [open_out], but the file is opened in binary mode, so that no
  translation takes place during writes. On operating systems that do not
  distinguish between text mode and binary mode, this func- tion behaves like
  [open_out].

  @raise Sys_error if the file could not be opened.
  @param enc is the encoding of the output channel [Encoding.locale]
  *)


  val open_out_gen : ?enc:Encoding.t ->
    open_flag list -> int -> string -> out_channel
  (** [open_out_gen mode perm filename] opens the named file for writing. The
  extra argument mode specify the opening mode. The extra argument perm
  specifies the file permissions, in case the file must be created.
  [open_out] and [open_out_bin] are special cases of this function.

  @raise Sys_error if the file could not be opened.
  @param enc is the encoding of the output channel [Encoding.locale]
  *)


  val open_in : ?enc:Encoding.t -> string -> in_channel
  (** Open the named file for reading, and return a new input channel on that
  file, positionned at the beginning of the file.

  @raise Sys_error if the file could not be opened.
  @param enc is the encoding of the output channel [Encoding.locale]
  *)


  val open_in_bin : ?enc:Encoding.t -> string -> in_channel
  (** Same as [open_in], but the file is opened in binary mode, so that no
  translation takes place during reads. On operating systems that do not
  distinguish between text mode and binary mode, this function behaves like
  [open_in].

  @raise Sys_error if the file could not be opened.
  @param enc is the encoding of the output channel [Encoding.locale]
  *)


  val open_in_gen : ?enc:Encoding.t ->
    open_flag list -> int -> string -> in_channel
  (** [open_in_gen mode perm filename] opens the named file for reading. The
  extra arguments [mode] and [perm] specify the opening mode and file
  permissions.  [open_in] and [open_in_bin] are special cases of this
  function.

  @raise Sys_error if the file could not be opened.
  @param enc is the encoding of the output channel [Encoding.locale]
  *)

  val input: in_channel -> uchar
  (** Input a value. May rise [End_of_file]. *)

  val output: out_channel -> uchar -> unit
  (** Output a value. *)

  val close_in: in_channel -> unit
  (** Close the channel. *)

  val close_out: out_channel -> unit
  (** Close the channel. *)

  val flush: out_channel -> unit
  (** Flush the channel. *)


  (** {2 Unicode characters I/O} *)

  val output_uchar: out_channel -> uchar -> unit
  val output_ustring: out_channel -> ustring -> unit
  val output_line: out_channel -> ustring -> unit
  val output_char: out_channel -> char -> unit
  val output_string: out_channel -> string -> unit
  val output_bool: out_channel -> bool -> unit
  val output_int: out_channel -> int -> unit
  val output_float: out_channel -> float -> unit
  val output_newline: out_channel -> unit
  val output_space: out_channel -> unit
  val output_htab: out_channel -> unit
  val output_vtab: out_channel -> unit
  val output_par: out_channel-> unit

  val input_uchar : in_channel -> uchar
  val input_line : in_channel -> ustring
end


(** Standard Input/Output.

You can write your Unicode application using Input/Output as a functor
parametrised by [UChannel_stdio.S].  The bootstrapping code of the
application can then prepare the [UChannel_stdio.PARAMETER] module and
call the main code after having instantiated the functor. *)
module UChannel_stdio :
sig

  module type PARAMETER =
  sig
    val stdin: Pervasives.in_channel
    val stdin_enc: Encoding.t

    val stdout: Pervasives.out_channel
    val stdout_enc: Encoding.t

    val stderr: Pervasives.out_channel
    val stderr_enc: Encoding.t
  end

  module type S =
  sig

    val ustdin: UChannel.in_channel
    val ustdout: UChannel.out_channel
    val ustderr: UChannel.out_channel

    val print_uchar: uchar -> unit
    val print_ustring: ustring -> unit
    val print_line: ustring -> unit
    val print_char: char -> unit
    val print_string: string -> unit
    val print_int: int -> unit
    val print_bool: bool -> unit
    val print_float: float -> unit
    val print_newline: unit -> unit
    val print_space: unit -> unit
    val print_htab: unit -> unit
    val print_vtab: unit -> unit
    val print_par: unit-> unit

    val prerr_uchar: uchar -> unit
    val prerr_ustring: ustring -> unit
    val prerr_line: ustring -> unit
    val prerr_char: char -> unit
    val prerr_string: string -> unit
    val prerr_int: int -> unit
    val prerr_bool: bool -> unit
    val prerr_float: float -> unit
    val prerr_newline: unit -> unit
    val prerr_space: unit -> unit
    val prerr_htab: unit -> unit
    val prerr_vtab: unit -> unit
    val prerr_par: unit-> unit

    val flush_std: unit -> unit

    val read_uchar : unit -> uchar
    val read_line : unit -> ustring
  end

  module Make(P:PARAMETER): S

end

(** Standard Input/Output adapted to locale.

It deserves the same purpose as [UChannel_stdio] but it automatically
tuned to the active locale, as reported from [Encoding.locale]. *)
module UChannel_locale :
sig

  module type PARAMETER =
  sig
    val stdin: Pervasives.in_channel
    val stdout: Pervasives.out_channel
    val stderr: Pervasives.out_channel
  end

  module type S = UChannel_stdio.S

  module Make(P:PARAMETER): S
end


(** Buffers containing unicode characters *)
module UBuffer :
sig
  type t

  val create : int -> t
  (** [create n] creates the buffer which initially can contain
  [n] Unicode characters. *)

  val contents : t -> ustring
  (** [contents b] returns a copy of the current content of the buffer. The
  buffer itself is unchanged. *)

  val sub: t -> int -> int -> ustring
  (** [sub b off len] returns a copy of the substring of the current content
  of the buffer, starting at position [off] and for [len] characters.

  @raise Invalid_argument if out of bounds request.
  *)

  val nth: t -> int -> uchar
  (** [nth b n] gets the [n]-th character in buffer [b].

  @raise Invalid_argument if out of bounds request.
  *)

  val length : t -> int
  (** [length b] returns the number of characters currently contained in the
  buffer. *)

  val clear : t -> unit
  val reset : t -> unit
  val add_char : t -> char -> unit
  val add_string: t -> string -> unit
  val add_uchar : t -> uchar -> unit
  val add_ustring : t -> ustring -> unit
  val add_buffer : t -> t -> unit
  val output_buffer : UChannel.out_channel -> t -> unit

  val join : t -> ustring -> ustring list -> unit
  (** [join b sep l] adds to [b] the elements of [l], separated by [sep]. *)

  val create : int -> t
  (** [create n] creates the buffer which initially can contain
  [n] Unicode characters. *)

  val contents : t -> ustring
  (** [contents b] returns a copy of the current content of the buffer. The
  buffer itself is unchanged. *)

  val sub: t -> int -> int -> ustring
  (** [sub b off len] returns a copy of the substring of the current content
  of the buffer, starting at position [off] and for [len] characters.

  @raise Invalid_argument if out of bounds request.
  *)

  val nth: t -> int -> uchar
  (** [nth b n] gets the [n]-th character in buffer [b].

  @raise Invalid_argument if out of bounds request.
  *)

  val length : t -> int
  (** [length b] returns the number of characters currently contained in the
  buffer. *)

  val clear : t -> unit
  val reset : t -> unit
  val add_char : t -> char -> unit
  val add_string: t -> string -> unit
  val add_uchar : t -> uchar -> unit
  val add_ustring : t -> ustring -> unit
  val add_buffer : t -> t -> unit
  val output_buffer : UChannel.out_channel -> t -> unit

  val join : t -> ustring -> ustring list -> unit
  (** [join b sep l] adds to [b] the elements of [l], separated by [sep]. *)

end


(** {2 Convenience functions} *)

val u: string -> ustring
(** Conversion from Latin-1 strings. *)

val u8: string -> ustring
(** Conversion from UTF-8 strings. *)

val escaped_uchar: uchar -> string
(** Escape a unicode character. *)
