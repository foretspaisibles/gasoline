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
open Format

module XString = CamomileLibrary.XString
module Camomile = CamomileLibraryDefault.Camomile
module Camomile_set = Camomile.USet
module Camomile_map = Camomile.UMap
module Camomile_char = Camomile.UChar
module Camomile_text =
struct
  include Array
  type mutability = [ `Mutable | `Immutable ]
  type 'a text = Camomile_char.t array
  type utext = [`Imutable] text
  type t = utext
  type ustring = [`Mutable] text
  let utext_of_ustring = Array.copy
  let ustring_of_utext = Array.copy
  type index = int
  let look s i = get s i
  let nth _ i = i
  let first _ = 0
  let last s = Array.length s - 1
  let out_of_range s i = i < 0 || i >= Array.length s
  let next _ i = i + 1
  let prev _ i = i - 1
  let move _ i n = i + n
  let compare_index _ (i : int) (j : int) = i - j
  let make len init = Array.make len init
  let init_ustring = init
  let of_string s = init (String.length s)
    (fun i -> Camomile_char.of_char s.[i])
  let of_xstring s = init (XString.length s)
    (fun i -> XString.get s i)
  let rec compare_aux i t1 t2 =
    if i >= length t1 then
      if i >= length t2 then 0 else ~-1
    else if i >= length t2 then 1 else
    match Camomile_char.compare (get t1 i) (get t2 i) with
      0 -> compare_aux (i + 1) t1 t2
    | sgn -> sgn

  let compare t1 t2 = compare_aux 0 t1 t2

  module Buf =
  struct
    include XString
    type buf = xstring
    let create bufsize = XString.make ~bufsize 0 (Camomile_char.chr_of_uint 0)
    let contents = of_xstring
    let contents_string = of_xstring
    let add_char = add_char
    let add_string b s =
      Array.iter (fun c -> add_char b c) s
    let add_buffer = add_xstring
  end
end

module Camomile_buffer = Camomile_text.Buf
module Camomile_information = Camomile.UCharInfo
module Camomile_table = Camomile.UCharTbl
module Camomile_channel = Camomile.OOChannel
module Camomile_encoding = Camomile.CharEncoding
module Camomile_transcode = Camomile.CharEncoding.Make(Camomile_text)
module Camomile_casemap = Camomile.CaseMap.Make(Camomile_text)
module Camomile_pervasives = Camomile.UPervasives

module Camomile_table_bool =
struct
  include Camomile.UCharTbl.Bool
  type elt = bool
end

module Camomile_table_bits =
struct
  include Camomile.UCharTbl.Bits
  type elt = int
end

module Camomile_table_bytes =
struct
  include Camomile.UCharTbl.Bytes
  type elt = int
end

module Camomile_table_char =
struct
  include Camomile.UCharTbl.Char
  type elt = char
end

type mutability = [ `Mutable | `Immutable ]

type uchar = Camomile_char.t
type 'a data = 'a Camomile_text.text
type udata = mutability data
type utext = [`Immutable] data
type ustring = [`Mutable] data

module USet =
struct
  include Camomile_set
  type elt = uchar
  let split u s = (before u s, mem u s, after u s)
end

module UMap =
struct
  type key = uchar
  include Camomile_map

  let add a b c = add a b c
  let add_range a b c d = add_range a b c d
  let map a b = map a b
  let mapi a b = mapi a b

  let constant = set_to_map
  let subdomain = map_to_set

  let rec compare c a b =
    let ka = USet.min_elt (domain a) in
    let kb = USet.min_elt (domain b) in
      compare_keys c a b (Camomile_char.compare ka kb) ka kb

  and compare_keys c a b kc ka kb =
    if kc = 0
    then compare_values c a b (c (find ka a) (find kb b)) ka kb
    else kc

  and compare_values c a b vc ka kb =
    if vc = 0
    then compare c (remove ka a) (remove kb b)
    else vc

  let equal c a b =
    compare (fun x y -> if c x y then 0 else 1) a b = 0

end

module UChar =
struct
  include Camomile_char
  let to_int = int_of
  let uint_chr = chr_of_uint
  let to_char = char_of

  let newline = of_char '\n'
  let space = of_char ' '
  let htab = of_char '\t'
  let vtab = of_char '\x0b'
  let nextline = of_int 0x0085
  let line = of_int 0x2028
  let par = of_int 0x2029

end

module type UTABLE =
sig
  type t
  type elt
  val get: t -> uchar -> elt
end

module UTable = Camomile_table
module UTable_char = Camomile_table_char
module UTable_bytes = Camomile_table_bytes
module UTable_bits = Camomile_table_bits
module UTable_bool = Camomile_table_bool

module UInformation =
struct
  type primary_category =
    | LETTER
    | MARK
    | NUMBER
    | SEPARATOR
    | OTHER
    | PUNCTUATION
    | SYMBOL

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

  type character_property =
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

  type casemap_condition =
    | AFTER_SOFT_DOTTED
    | BEFORE_DOT
    | FINAL_SIGMA
    | LOCALE of string
    | MORE_ABOVE
    | NOT of casemap_condition

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

  type decomposition_info =
    | CANON_FORM
    | COMPOSITE of decomposition * uchar list
    | HANGUL_SYLLABLE


  let general_category_import =
    function
      |`Lu -> LETTER_UPPERCASE
      |`Ll -> LETTER_LOWERCASE
      |`Lt -> LETTER_TITLECASE
      |`Mn -> MARK_NON_SPACING
      |`Mc -> MARK_SPACING_COMBINING
      |`Me -> MARK_ENCLOSING
      |`Nd -> NUMBER_DECIMAL_DIGIT
      |`Nl -> NUMBER_LETTER
      |`No -> NUMBER_OTHER
      |`Zs -> SEPARATOR_SPACE
      |`Zl -> SEPARATOR_LINE
      |`Zp -> SEPARATOR_PARAGRAPH
      |`Cc -> OTHER_CONTROL
      |`Cf -> OTHER_FORMAT
      |`Cs -> OTHER_SURROGATE
      |`Co -> OTHER_PRIVATE_USE
      |`Cn -> OTHER_NOT_ASSIGNED
      |`Lm -> LETTER_MODIFIER
      |`Lo -> LETTER_OTHER
      |`Pc -> PUNCTUATION_CONNECTOR
      |`Pd -> PUNCTUATION_DASH
      |`Ps -> PUNCTUATION_OPEN
      |`Pe -> PUNCTUATION_CLOSE
      |`Pi -> PUNCTUATION_INITIAL
      |`Pf -> PUNCTUATION_FINAL
      |`Po -> PUNCTUATION_OTHER
      |`Sm -> SYMBOL_MATH
      |`Sc -> SYMBOL_CURRENCY
      |`Sk -> SYMBOL_MODIFIER
      |`So -> SYMBOL_OTHER

  let general_category_export =
    function
      | LETTER_UPPERCASE -> `Lu
      | LETTER_LOWERCASE -> `Ll
      | LETTER_TITLECASE -> `Lt
      | MARK_NON_SPACING -> `Mn
      | MARK_SPACING_COMBINING -> `Mc
      | MARK_ENCLOSING -> `Me
      | NUMBER_DECIMAL_DIGIT -> `Nd
      | NUMBER_LETTER -> `Nl
      | NUMBER_OTHER -> `No
      | SEPARATOR_SPACE -> `Zs
      | SEPARATOR_LINE -> `Zl
      | SEPARATOR_PARAGRAPH -> `Zp
      | OTHER_CONTROL -> `Cc
      | OTHER_FORMAT -> `Cf
      | OTHER_SURROGATE -> `Cs
      | OTHER_PRIVATE_USE -> `Co
      | OTHER_NOT_ASSIGNED -> `Cn
      | LETTER_MODIFIER -> `Lm
      | LETTER_OTHER -> `Lo
      | PUNCTUATION_CONNECTOR -> `Pc
      | PUNCTUATION_DASH -> `Pd
      | PUNCTUATION_OPEN -> `Ps
      | PUNCTUATION_CLOSE -> `Pe
      | PUNCTUATION_INITIAL  -> `Pi
      | PUNCTUATION_FINAL  -> `Pf
      | PUNCTUATION_OTHER -> `Po
      | SYMBOL_MATH -> `Sm
      | SYMBOL_CURRENCY -> `Sc
      | SYMBOL_MODIFIER -> `Sk
      | SYMBOL_OTHER -> `So

  let character_property_import =
    function
      | `Math -> MATH
      | `Alphabetic -> ALPHABETIC
      | `Lowercase -> LOWERCASE
      | `Uppercase -> UPPERCASE
      | `ID_Start -> ID_START
      | `ID_Continue -> ID_CONTINUE
      | `XID_Start -> XID_START
      | `XID_Continue -> XID_CONTINUE
      | `Default_Ignorable_Code_Point -> DEFAULT_IGNORABLE_CODE_POINT
      | `Grapheme_Extend -> GRAPHEME_EXTEND
      | `Grapheme_Base -> GRAPHEME_BASE
      | `Bidi_Control -> BIDI_CONTROL
      | `White_Space -> WHITE_SPACE
      | `Hyphen -> HYPHEN
      | `Quotation_Mark -> QUOTATION_MARK
      | `Terminal_Punctuation -> TERMINAL_PUNCTUATION
      | `Other_Math -> OTHER_MATH
      | `Hex_Digit -> HEX_DIGIT
      | `Ascii_Hex_Digit -> ASCII_HEX_DIGIT
      | `Other_Alphabetic -> OTHER_ALPHABETIC
      | `Ideographic -> IDEOGRAPHIC
      | `Diacritic -> DIACRITIC
      | `Extender -> EXTENDER
      | `Other_Lowercase -> OTHER_LOWERCASE
      | `Other_Uppercase -> OTHER_UPPERCASE
      | `Noncharacter_Code_Point -> NONCHARACTER_CODE_POINT
      | `Grapheme_Link -> GRAPHEME_LINK
      | `Other_Grapheme_Extend -> OTHER_GRAPHEME_EXTEND
      | `IDS_Binary_Operator -> IDS_BINARY_OPERATOR
      | `IDS_Trinary_Operator -> IDS_TRINARY_OPERATOR
      | `Radical -> RADICAL
      | `Unified_Ideograph -> UNIFIED_IDEOGRAPH
      | `Other_default_Ignorable_Code_Point ->
	  OTHER_DEFAULT_IGNORABLE_CODE_POINT
      | `Deprecated -> DEPRECATED
      | `Soft_Dotted -> SOFT_DOTTED
      | `Logical_Order_Exception -> LOGICAL_ORDER_EXCEPTION

  let character_property_export =
    function
      | MATH -> `Math
      | ALPHABETIC -> `Alphabetic
      | LOWERCASE -> `Lowercase
      | UPPERCASE -> `Uppercase
      | ID_START -> `ID_Start
      | ID_CONTINUE -> `ID_Continue
      | XID_START -> `XID_Start
      | XID_CONTINUE -> `XID_Continue
      | DEFAULT_IGNORABLE_CODE_POINT -> `Default_Ignorable_Code_Point
      | GRAPHEME_EXTEND -> `Grapheme_Extend
      | GRAPHEME_BASE -> `Grapheme_Base
      | BIDI_CONTROL -> `Bidi_Control
      | WHITE_SPACE -> `White_Space
      | HYPHEN -> `Hyphen
      | QUOTATION_MARK -> `Quotation_Mark
      | TERMINAL_PUNCTUATION -> `Terminal_Punctuation
      | OTHER_MATH -> `Other_Math
      | HEX_DIGIT -> `Hex_Digit
      | ASCII_HEX_DIGIT -> `Ascii_Hex_Digit
      | OTHER_ALPHABETIC -> `Other_Alphabetic
      | IDEOGRAPHIC -> `Ideographic
      | DIACRITIC -> `Diacritic
      | EXTENDER -> `Extender
      | OTHER_LOWERCASE -> `Other_Lowercase
      | OTHER_UPPERCASE -> `Other_Uppercase
      | NONCHARACTER_CODE_POINT -> `Noncharacter_Code_Point
      | GRAPHEME_LINK -> `Grapheme_Link
      | OTHER_GRAPHEME_EXTEND -> `Other_Grapheme_Extend
      | IDS_BINARY_OPERATOR -> `IDS_Binary_Operator
      | IDS_TRINARY_OPERATOR -> `IDS_Trinary_Operator
      | RADICAL -> `Radical
      | UNIFIED_IDEOGRAPH -> `Unified_Ideograph
      | OTHER_DEFAULT_IGNORABLE_CODE_POINT ->
	  `Other_default_Ignorable_Code_Point
      | DEPRECATED -> `Deprecated
      | SOFT_DOTTED -> `Soft_Dotted
      | LOGICAL_ORDER_EXCEPTION -> `Logical_Order_Exception

  let script_import =
    function
      | `Common -> COMMON
      | `Inherited -> INHERITED
      | `Latin -> LATIN
      | `Greek -> GREEK
      | `Cyrillic -> CYRILLIC
      | `Armenian -> ARMENIAN
      | `Hebrew -> HEBREW
      | `Arabic -> ARABIC
      | `Syriac -> SYRIAC
      | `Thaana -> THAANA
      | `Devanagari -> DEVANAGARI
      | `Bengali -> BENGALI
      | `Gurmukhi -> GURMUKHI
      | `Gujarati -> GUJARATI
      | `Oriya -> ORIYA
      | `Tamil -> TAMIL
      | `Telugu -> TELUGU
      | `Kannada -> KANNADA
      | `Malayalam -> MALAYALAM
      | `Sinhala -> SINHALA
      | `Thai -> THAI
      | `Lao -> LAO
      | `Tibetan -> TIBETAN
      | `Myanmar -> MYANMAR
      | `Georgian -> GEORGIAN
      | `Hangul -> HANGUL
      | `Ethiopic -> ETHIOPIC
      | `Cherokee -> CHEROKEE
      | `Canadian_Aboriginal -> CANADIAN_ABORIGINAL
      | `Ogham -> OGHAM
      | `Runic -> RUNIC
      | `Khmer -> KHMER
      | `Mongolian -> MONGOLIAN
      | `Hiragana -> HIRAGANA
      | `Katakana -> KATAKANA
      | `Bopomofo -> BOPOMOFO
      | `Han -> HAN
      | `Yi -> YI
      | `Old_Italic -> OLD_ITALIC
      | `Gothic -> GOTHIC
      | `Deseret -> DESERET
      | `Tagalog -> TAGALOG
      | `Hanunoo -> HANUNOO
      | `Buhid -> BUHID
      | `Tagbanwa -> TAGBANWA

  let script_export =
    function
      | COMMON -> `Common
      | INHERITED -> `Inherited
      | LATIN -> `Latin
      | GREEK -> `Greek
      | CYRILLIC -> `Cyrillic
      | ARMENIAN -> `Armenian
      | HEBREW -> `Hebrew
      | ARABIC -> `Arabic
      | SYRIAC -> `Syriac
      | THAANA -> `Thaana
      | DEVANAGARI -> `Devanagari
      | BENGALI -> `Bengali
      | GURMUKHI -> `Gurmukhi
      | GUJARATI -> `Gujarati
      | ORIYA -> `Oriya
      | TAMIL -> `Tamil
      | TELUGU -> `Telugu
      | KANNADA -> `Kannada
      | MALAYALAM -> `Malayalam
      | SINHALA -> `Sinhala
      | THAI -> `Thai
      | LAO -> `Lao
      | TIBETAN -> `Tibetan
      | MYANMAR -> `Myanmar
      | GEORGIAN -> `Georgian
      | HANGUL -> `Hangul
      | ETHIOPIC -> `Ethiopic
      | CHEROKEE -> `Cherokee
      | CANADIAN_ABORIGINAL -> `Canadian_Aboriginal
      | OGHAM -> `Ogham
      | RUNIC -> `Runic
      | KHMER -> `Khmer
      | MONGOLIAN -> `Mongolian
      | HIRAGANA -> `Hiragana
      | KATAKANA -> `Katakana
      | BOPOMOFO -> `Bopomofo
      | HAN -> `Han
      | YI -> `Yi
      | OLD_ITALIC -> `Old_Italic
      | GOTHIC -> `Gothic
      | DESERET -> `Deseret
      | TAGALOG -> `Tagalog
      | HANUNOO -> `Hanunoo
      | BUHID -> `Buhid
      | TAGBANWA -> `Tagbanwa

  let general_category_to_primary_category =
    function
      | LETTER_UPPERCASE -> LETTER
      | LETTER_LOWERCASE -> LETTER
      | LETTER_TITLECASE -> LETTER
      | MARK_NON_SPACING -> MARK
      | MARK_SPACING_COMBINING -> MARK
      | MARK_ENCLOSING -> MARK
      | NUMBER_DECIMAL_DIGIT -> NUMBER
      | NUMBER_LETTER -> NUMBER
      | NUMBER_OTHER -> NUMBER
      | SEPARATOR_SPACE -> SEPARATOR
      | SEPARATOR_LINE -> SEPARATOR
      | SEPARATOR_PARAGRAPH -> SEPARATOR
      | OTHER_CONTROL -> OTHER
      | OTHER_FORMAT -> OTHER
      | OTHER_SURROGATE -> OTHER
      | OTHER_PRIVATE_USE -> OTHER
      | OTHER_NOT_ASSIGNED -> OTHER
      | LETTER_MODIFIER -> LETTER
      | LETTER_OTHER -> LETTER
      | PUNCTUATION_CONNECTOR -> PUNCTUATION
      | PUNCTUATION_DASH -> PUNCTUATION
      | PUNCTUATION_OPEN -> PUNCTUATION
      | PUNCTUATION_CLOSE -> PUNCTUATION
      | PUNCTUATION_INITIAL  -> PUNCTUATION
      | PUNCTUATION_FINAL  -> PUNCTUATION
      | PUNCTUATION_OTHER -> PUNCTUATION
      | SYMBOL_MATH -> SYMBOL
      | SYMBOL_CURRENCY -> SYMBOL
      | SYMBOL_MODIFIER -> SYMBOL
      | SYMBOL_OTHER -> SYMBOL

  let general_category x =
    general_category_import(Camomile_information.general_category x)

  let primary_category x =
    general_category_to_primary_category (general_category x)

  let load_general_category_map () =
    UMap.map general_category_import (
      Camomile_information.load_general_category_map ()
    )

  let load_property_table p =
    Camomile_information.load_property_tbl (
      character_property_export p
    )

  let load_property_set p =
    Camomile_information.load_property_set (
      character_property_export p
    )

  let load_property_table_by_name n =
    Camomile_information.load_property_tbl_by_name n

  let load_property_set_by_name n =
    Camomile_information.load_property_set_by_name n

  let script c =
    script_import(Camomile_information.script c)

  let load_script_map () =
    UMap.map script_import (
      Camomile_information.load_script_map ()
    )

  let lower_table = Camomile_information.load_to_lower1_tbl ()
  let upper_table = Camomile_information.load_to_upper1_tbl ()
  let title_table = Camomile_information.load_to_title1_tbl ()

  let test_primary_category c x =
    primary_category x = c

  let test_general_category c x =
    general_category x = c

  let is_letter =
    test_primary_category LETTER

  let is_alpha = is_letter

  let is_mark =
    test_primary_category MARK

  let is_number =
    test_primary_category NUMBER

  let is_punctuation =
    test_primary_category PUNCTUATION

  let is_symbol =
    test_primary_category SYMBOL

  let is_other =
    test_primary_category OTHER

  let is_separator =
    test_primary_category SEPARATOR

  let is_space x =
    match general_category x with
      | MARK_SPACING_COMBINING
      | SEPARATOR_SPACE
      | SEPARATOR_LINE
      | SEPARATOR_PARAGRAPH -> true
      | _ -> false

  let is_cntrl =
    test_general_category OTHER_CONTROL

  let is_ascii x =
    let c = UChar.uint_code x in
      c <= 127 && 0 <= c

  let is_latin x =
      (script x) = LATIN

  let is_upper =
    test_general_category LETTER_UPPERCASE

  let is_lower =
    test_general_category LETTER_LOWERCASE

  let is_title =
    test_general_category LETTER_TITLECASE

  let is_graph x =
    not(test_primary_category OTHER x)

  let is_print x =
    is_graph x && not (is_space x)

  let is_alnum x =
    is_alpha x || is_number x

  let is_digit =
    test_general_category NUMBER_DECIMAL_DIGIT

  let is_bdigit x =
    let c = UChar.uint_code x in
      48 <= c && c <= 49

  let is_xdigit x =
    let c = UChar.uint_code x in
      (48 <= c && c <= 57)||(65 <= c && c <= 70)||(97 <= c && c <= 102)

  let is_odigit x =
    let c = UChar.uint_code x in
      (48 <= c && c <= 55)

  let is_newline x =
    10 = UChar.uint_code x

  let is_zero x =
    0 = UChar.uint_code x

  let table_get t c =
    let a = UTable.get t c in
      if is_zero a then c else a

  let to_lower c = table_get lower_table c
  let to_upper c = table_get upper_table c
  let to_title c = table_get title_table c

end

module Encoding =
struct
  exception Malformed_code = Camomile_encoding.Malformed_code
  exception Out_of_range = Camomile_encoding.Out_of_range
  type t = Camomile_encoding.t

  let automatic = Camomile_encoding.automatic
  let register = Camomile_encoding.new_enc
  let alias = Camomile_encoding.alias
  let find = Camomile_encoding.of_name
  let name = Camomile_encoding.name_of

  let ascii = Camomile_encoding.ascii
  let latin1 = Camomile_encoding.latin1
  let iso_8859_15 = find "ISO-8859-15"
  let _ = alias "Latin9" "ISO-8859-15"
  let latin9 = find "Latin9"
  let utf8 = Camomile_encoding.utf8
  let utf16 = Camomile_encoding.utf16
  let utf16be = Camomile_encoding.utf16be
  let utf16le = Camomile_encoding.utf16le
  let utf32 = Camomile_encoding.utf32
  let utf32be = Camomile_encoding.utf32be
  let utf32le = Camomile_encoding.utf32le
  let ucs4 = Camomile_encoding.ucs4

  let locale =
    try
      let l = Sys.getenv "LANG" in
      let i = String.index l '.' in
	find (String.sub l (i+1) (String.length l - i - 1))
    with _ -> latin9

  let recode_string = Camomile_encoding.recode_string

  let decode = Camomile_transcode.decode
  let encode = Camomile_transcode.encode

  let import = Camomile_transcode.decode locale
  let export = Camomile_transcode.encode locale

end

module UString =
struct
  type t = ustring
  type index = Camomile_text.index

  let get = Camomile_text.get
  let set = Camomile_text.set
  let copy = Camomile_text.copy
  let compare = Camomile_text.compare
  let equal a b =
    compare a b = 0
  let iter = Camomile_text.iter
  let append = Camomile_text.append
  let blit = Camomile_text.blit
  let fill = Camomile_text.fill
  let sub = Camomile_text.sub
  let make = Camomile_text.make
  let init_ustring = Camomile_text.init_ustring
  let init_utext = Camomile_text.init
  let init = init_utext
  let of_string = Encoding.decode Encoding.latin9
  let of_uchar c = make 1 c
  let of_char c = of_uchar (UChar.of_char c)
  let length = Camomile_text.length
  let move = Camomile_text.move
  let prev = Camomile_text.prev
  let next = Camomile_text.next
  let compare_index = Camomile_text.compare_index
  let out_of_range = Camomile_text.out_of_range
  let last = Camomile_text.last
  let first = Camomile_text.first
  let nth = Camomile_text.nth
  let look = Camomile_text.look


  let uempty = of_string ""

  let concat l =
    List.fold_left append uempty l

  let rec join_insert s x l = match l with
    | [] -> x
    | h :: [] -> h :: x
    | h :: (_ :: _) as t -> join_insert s (s :: h :: x) t

  let join sep l =
    concat (List.rev (join_insert sep [] l))

  let lowercase x = Camomile_casemap.lowercase x
  let uppercase x = Camomile_casemap.uppercase x
  let titlecase x = Camomile_casemap.titlecase x

  let transform f x =
    let x' = copy x in
      if length x' > 0 then set x' 0 (f (get x' 0));
      x'

  let capitalize = transform UInformation.to_upper

  let uncapitalize = transform UInformation.to_lower

  let format e f u =
    pp_print_string f (Encoding.encode e u)

  let output e c u =
    output_string c (Encoding.encode e u)

  let print e u =
    output e stdout u

  let prerr e u =
    output e stderr u

end


module UChannel =
struct

  let in_buffer_sz = 128

  type in_channel = {
    mutable in_open: bool;
    in_buffer: Camomile_buffer.t;
    in_backend: uchar Camomile_channel.obj_input_channel;
  }

  type out_channel = {
    mutable out_open: bool;
    out_backend: uchar Camomile_channel.obj_output_channel;
  }

  let close_in c =
    if c.in_open then begin
      c.in_backend#close_in ();
      c.in_open <- false;
    end

  let close_out c =
    if c.out_open then begin
      c.out_backend#close_out ();
      c.out_open <- false;
    end

  class type block_out_channel =
  object
    method close_out : unit -> unit
    method flush : unit -> unit
    method output : string -> int -> int -> int
  end

  class type block_in_channel =
  object
    method close_in : unit -> unit
    method input : string -> int -> int -> int
  end

  let with_finalise f a =
    Gc.finalise f a; a

  let from_in_backend backend =
    with_finalise close_in {
      in_open = true;
      in_buffer = Camomile_buffer.create in_buffer_sz;
      in_backend = backend;
    }

  let from_out_backend backend =
    with_finalise close_out {
      out_open = true;
      out_backend = backend;
    }

  let from_block_in_channel ?(enc = Encoding.locale) p =
    from_in_backend (new Camomile_encoding.uchar_input_channel_of enc p)

  let from_block_out_channel ?(enc = Encoding.locale) p =
    from_out_backend (new Camomile_encoding.uchar_output_channel_of enc p)

  let from_in_channel ?(enc = Encoding.locale) p =
    from_in_backend (new Camomile_encoding.in_channel enc p)

  let from_out_channel ?(enc = Encoding.locale) p =
    from_out_backend (new Camomile_encoding.out_channel enc p)

  let input c =
    c.in_backend#get ()

  let output c x =
    c.out_backend#put x

  let flush c =
    c.out_backend#flush ()

  class adapt_formatter f =
  object

    method output s a b =
      for i = 0 to b - 1 do
	  pp_print_char f s.[a + i]
      done; b

    method flush () =
      pp_print_flush f ()

    method close_out () =
      pp_print_flush f ()

  end

  let from_formatter ?enc p =
    from_block_out_channel ?enc (new adapt_formatter p)

  let open_out ?enc f =
    from_out_channel ?enc (open_out f)

  let open_out_bin ?enc f =
    from_out_channel ?enc (open_out_bin f)

  let open_out_gen ?enc l p f =
    from_out_channel ?enc (open_out_gen l p f)

  let open_in ?enc f =
    from_in_channel ?enc (open_in f)

  let open_in_bin ?enc f =
    from_in_channel ?enc (open_in_bin f)

  let open_in_gen ?enc l p f =
    from_in_channel ?enc (open_in_gen l p f)

  let output_uchar c u =
    output c u

  let output_ustring c s =
    UString.iter (output_uchar c) s

  let output_char c l =
    output_uchar c (UChar.of_char l)

  let output_string c s =
    String.iter (output_char c) s

  let output_bool c b =
    output_string c (string_of_bool b)

  let output_int c x =
    output_string c (string_of_int x)

  let output_float c x =
    output_string c (string_of_float x)

  let output_newline c =
    output_uchar c UChar.newline

  let output_space c =
    output_uchar c UChar.space

  let output_htab c =
    output_uchar c UChar.htab

  let output_vtab c =
    output_uchar c UChar.vtab

  let output_par c =
    output_uchar c UChar.par

  let output_line c s =
    output_ustring c s;
    output_newline c

  let input_uchar c =
    input c

  let input_line c =
    let answer =
      begin
	try while true do
	    let x = input_uchar c in
	    if UInformation.is_newline x then raise Exit;
	    Camomile_buffer.add_char c.in_buffer x
	  done;
	with Exit | End_of_file -> ()
      end;
      Camomile_buffer.contents c.in_buffer
    in begin
      Camomile_buffer.reset c.in_buffer;
      answer
    end

end

module UFormat =
struct

  type formatter = {
    mutable encoding: Encoding.t;
    mutable formatter: Format.formatter;
  }

  type tag = string

  type formatter_tag_functions = {
    mark_open_tag : tag -> string;
    mark_close_tag : tag -> string;
    print_open_tag : tag -> unit;
    print_close_tag : tag -> unit;
  }

  let tags_import x = {
    mark_open_tag = x.Format.mark_open_tag;
    mark_close_tag = x.Format.mark_close_tag;
    print_open_tag = x.Format.print_open_tag;
    print_close_tag = x.Format.print_close_tag;
  }

  let tags_export x = {
    Format.mark_open_tag = x.mark_open_tag;
    Format.mark_close_tag = x.mark_close_tag;
    Format.print_open_tag = x.print_open_tag;
    Format.print_close_tag = x.print_close_tag;
  }

  let create ?(enc = Encoding.locale ) f =
    {
      encoding = enc;
      formatter = f;
    }

  let formatter_of_formatter =
    create

  let formatter_of_out_channel ?enc c =
    create ?enc (Format.formatter_of_out_channel c)

  let formatter_of_buffer ?enc b =
    create ?enc (Format.formatter_of_buffer b)

  let stdbuf =
    Format.stdbuf

  let str_formatter =
    create Format.str_formatter

  let flush_str_formatter =
    Format.flush_str_formatter

  let make_formatter ?enc output flush =
    create ?enc (Format.make_formatter output flush)

  let pp_open_hbox f z =
    Format.pp_open_hbox f.formatter z

  let pp_open_vbox f z =
    Format.pp_open_vbox f.formatter z

  let pp_open_hvbox f z =
    Format.pp_open_hvbox f.formatter z

  let pp_open_hovbox f z =
    Format.pp_open_hovbox f.formatter z

  let pp_open_box f z =
    Format.pp_open_box f.formatter z

  let pp_close_box f z =
    Format.pp_close_box f.formatter z

  let pp_open_tag f z =
    Format.pp_open_tag f.formatter z

  let pp_close_tag f z =
    Format.pp_close_tag f.formatter z

  let pp_print_string f z =
    Format.pp_print_string f.formatter z

  let pp_print_ustring f z =
    pp_print_string f (Encoding.encode f.encoding z)

  let pp_print_as f y z =
    Format.pp_print_as f.formatter y z

  let pp_print_int f z =
    Format.pp_print_int f.formatter z

  let pp_print_float f z =
    Format.pp_print_float f.formatter z

  let pp_print_char f z =
    Format.pp_print_char f.formatter z

  let pp_print_uchar f z =
    pp_print_ustring f (UString.of_uchar z)

  let pp_print_bool f z =
    Format.pp_print_bool f.formatter z

  let pp_print_break f y z =
    Format.pp_print_break f.formatter y z

  let pp_print_cut f z =
    Format.pp_print_cut f.formatter z

  let pp_print_space f z =
    Format.pp_print_space f.formatter z

  let pp_force_newline f z =
    Format.pp_force_newline f.formatter z

  let pp_print_flush f z =
    Format.pp_print_flush f.formatter z

  let pp_print_newline f z =
    Format.pp_print_newline f.formatter z

  let pp_print_if_newline f z =
    Format.pp_print_if_newline f.formatter z

  let pp_open_tbox f z =
    Format.pp_open_tbox f.formatter z

  let pp_close_tbox f z =
    Format.pp_close_tbox f.formatter z

  let pp_print_tbreak f x z =
    Format.pp_print_tbreak f.formatter x z

  let pp_set_tab f z =
    Format.pp_set_tab f.formatter z

  let pp_print_tab f z =
    Format.pp_print_tab f.formatter z

  let pp_set_tags f z =
    Format.pp_set_tags f.formatter z

  let pp_set_print_tags f z =
    Format.pp_set_print_tags f.formatter z

  let pp_set_mark_tags f z =
    Format.pp_set_mark_tags f.formatter z

  let pp_get_print_tags f z =
    Format.pp_get_print_tags f.formatter z

  let pp_get_mark_tags f z =
    Format.pp_get_mark_tags f.formatter z

  let pp_set_margin f z =
    Format.pp_set_margin f.formatter z

  let pp_get_margin f z =
    Format.pp_get_margin f.formatter z

  let pp_set_max_indent f z =
    Format.pp_set_max_indent f.formatter z

  let pp_get_max_indent f z =
    Format.pp_get_max_indent f.formatter z

  let pp_set_max_boxes f z =
    Format.pp_set_max_boxes f.formatter z

  let pp_get_max_boxes f z =
    Format.pp_get_max_boxes f.formatter z

  let pp_over_max_boxes f z =
    Format.pp_over_max_boxes f.formatter z

  let pp_get_formatter_encoding f () =
    f.encoding

  let pp_set_formatter_encoding f e =
    f.encoding <- e

  let pp_set_ellipsis_text f s =
    Format.pp_set_ellipsis_text f.formatter s

  let pp_get_ellipsis_text f () =
    Format.pp_get_ellipsis_text f.formatter ()

  let pp_set_formatter_out_channel f c =
    Format.pp_set_formatter_out_channel f.formatter c

  let pp_set_formatter_output_functions f output flush =
    Format.pp_set_formatter_output_functions f.formatter output flush

  let pp_get_formatter_output_functions f =
    Format.pp_get_formatter_output_functions f.formatter

  let pp_set_all_formatter_output_functions f ~out ~flush ~newline ~spaces =
    Format.pp_set_all_formatter_output_functions f.formatter
      ~out ~flush ~newline ~spaces

  let pp_get_all_formatter_output_functions f () =
    Format.pp_get_all_formatter_output_functions f.formatter ()

  let pp_set_formatter_tag_functions f t =
    Format.pp_set_formatter_tag_functions f.formatter (tags_export t)

  let pp_get_formatter_tag_functions f () =
    tags_import (Format.pp_get_formatter_tag_functions f.formatter ())

  let kfprintf k f x =
    let callback _ = k f in
    Format.kfprintf callback f.formatter x

  let fprintf f x =
    Format.fprintf f.formatter x

  let bprintf b x =
    Format.bprintf b x

  let sprintf x =
    Format.sprintf x

  let ksprintf k f x =
    Format.ksprintf k f x

  let kprintf = ksprintf

    let ifprintf f x =
      Format.ifprintf f.formatter x
end

module UChannel_stdio =
struct

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

  module Make(P:PARAMETER) =
  struct

    open UChannel

    let ustdin = from_in_channel ~enc:P.stdin_enc P.stdin
    let ustdout = from_out_channel ~enc:P.stdout_enc P.stdout
    let ustderr = from_out_channel ~enc:P.stderr_enc P.stderr

    let print_uchar x =
      output_uchar ustdout x

    let print_ustring x =
      output_ustring ustdout x

    let print_char x =
      output_char ustdout x

    let print_string x =
      output_string ustdout x

    let print_int x =
      output_int ustdout x

    let print_bool x =
      output_bool ustdout x

    let print_float x =
      output_float ustdout x

    let print_newline () =
      output_newline ustdout;
      flush ustdout

    let print_space () =
      output_space ustdout

    let print_htab () =
      output_htab ustdout

    let print_vtab () =
      output_vtab ustdout

    let print_par () =
      output_par ustdout;
      flush ustdout

    let print_line s =
      output_line ustdout s;
      flush ustdout

    let prerr_uchar x =
      output_uchar ustderr x

    let prerr_ustring x =
      output_ustring ustderr x

    let prerr_char x =
      output_char ustderr x

    let prerr_string x =
      output_string ustderr x

    let prerr_int x =
      output_int ustderr x

    let prerr_bool x =
      output_bool ustderr x

    let prerr_float x =
      output_float ustderr x

    let prerr_newline () =
      output_newline ustderr;
      flush ustderr

    let prerr_space () =
      output_space ustderr

    let prerr_htab () =
      output_htab ustderr

    let prerr_vtab () =
      output_vtab ustderr

    let prerr_par () =
      output_par ustderr;
      flush ustderr

    let prerr_line s =
      output_line ustderr s;
      flush ustdout;
      flush ustderr

    let read_uchar () =
      input_uchar ustdin

    let read_line () =
      input_line ustdin

    let flush_std () =
      flush ustdout;
      flush ustderr

  end
end


module UChannel_locale =
struct

  module type PARAMETER =
  sig
    val stdin: Pervasives.in_channel
    val stdout: Pervasives.out_channel
    val stderr: Pervasives.out_channel
  end

  module type S = UChannel_stdio.S

  module Make(P:PARAMETER) =
  struct
    module Parameter =
    struct
      include P
      let stdin_enc = Encoding.locale
      let stdout_enc = Encoding.locale
      let stderr_enc = Encoding.locale
    end
    include UChannel_stdio.Make(Parameter)
  end

end

module UFormat_stdio =
struct


  module type PARAMETER =
  sig
    val ustdin: UChannel.in_channel
    val ustdout: UChannel.out_channel
    val ustderr: UChannel.out_channel
  end


  module type S =
  sig
    open UFormat
    val std_formatter : formatter
    val err_formatter : formatter

    val open_box : int -> unit
    val close_box : unit -> unit
    val print_string : string -> unit
    val print_ustring : ustring -> unit
    val print_as : int -> string -> unit
    val print_int : int -> unit
    val print_float : float -> unit
    val print_char : char -> unit
    val print_uchar : uchar -> unit
    val print_bool : bool -> unit
    val print_space : unit -> unit
    val print_cut : unit -> unit
    val print_break : int -> int -> unit
    val print_flush : unit -> unit
    val print_newline : unit -> unit
    val force_newline : unit -> unit
    val print_if_newline : unit -> unit
    val set_margin : int -> unit
    val get_margin : unit -> int
    val set_max_indent : int -> unit
    val get_max_indent : unit -> int
    val set_max_boxes : int -> unit
    val get_max_boxes : unit -> int
    val over_max_boxes : unit -> bool
    val open_hbox : unit -> unit
    val open_vbox : int -> unit
    val open_hvbox : int -> unit
    val open_hovbox : int -> unit
    val open_tbox : unit -> unit
    val close_tbox : unit -> unit
    val print_tbreak : int -> int -> unit
    val set_tab : unit -> unit
    val print_tab : unit -> unit
    val set_ellipsis_text : string -> unit
    val get_ellipsis_text : unit -> string

    val open_tag : tag -> unit
    val close_tag : unit -> unit
    val set_tags : bool -> unit
    val set_print_tags : bool -> unit
    val set_mark_tags : bool -> unit
    val get_print_tags : unit -> bool
    val get_mark_tags : unit -> bool

    val get_formatter_encoding : unit -> Encoding.t
    val set_formatter_encoding : Encoding.t -> unit

    val set_formatter_out_channel :
      Pervasives.out_channel -> unit
    val set_formatter_output_functions :
      (string -> int -> int -> unit) -> (unit -> unit) -> unit
    val get_formatter_output_functions :
      unit -> (string -> int -> int -> unit) * (unit -> unit)
    val set_formatter_tag_functions :
      formatter_tag_functions -> unit
    val get_formatter_tag_functions :
      unit -> formatter_tag_functions
    val set_all_formatter_output_functions :
      out:(string -> int -> int -> unit) ->
      flush:(unit -> unit) ->
      newline:(unit -> unit) -> spaces:(int -> unit) -> unit
    val get_all_formatter_output_functions :
      unit ->
      (string -> int -> int -> unit) * (unit -> unit) * (unit -> unit) *
	(int -> unit)
    val printf : ('a, Format.formatter, unit) Pervasives.format -> 'a
    val eprintf : ('a, Format.formatter, unit) Pervasives.format -> 'a

  end


  module Make(P:PARAMETER) =
  struct
    open UFormat

    let std_formatter =
      create Format.std_formatter

    let err_formatter =
      create Format.err_formatter

    let printf x =
      Format.fprintf std_formatter.formatter x

    let eprintf x =
      Format.fprintf err_formatter.formatter x

    let open_box =
      pp_open_box std_formatter
    let close_box =
      pp_close_box std_formatter
    let print_string =
      pp_print_string std_formatter
    let print_ustring =
      pp_print_ustring std_formatter
    let print_as =
      pp_print_as std_formatter
    let print_int =
      pp_print_int std_formatter
    let print_float =
      pp_print_float std_formatter
    let print_char =
      pp_print_char std_formatter
    let print_uchar =
      pp_print_uchar std_formatter
    let print_bool =
      pp_print_bool std_formatter
    let print_space =
      pp_print_space std_formatter
    let print_cut =
      pp_print_cut std_formatter
    let print_break =
      pp_print_break std_formatter
    let print_flush =
      pp_print_flush std_formatter
    let print_newline =
      pp_print_newline std_formatter
    let force_newline =
      pp_force_newline std_formatter
    let print_if_newline =
      pp_print_if_newline std_formatter
    let set_margin =
      pp_set_margin std_formatter
    let get_margin =
      pp_get_margin std_formatter
    let set_max_indent =
      pp_set_max_indent std_formatter
    let get_max_indent =
      pp_get_max_indent std_formatter
    let set_max_boxes =
      pp_set_max_boxes std_formatter
    let get_max_boxes =
      pp_get_max_boxes std_formatter
    let over_max_boxes =
      pp_over_max_boxes std_formatter
    let open_hbox =
      pp_open_hbox std_formatter
    let open_vbox =
      pp_open_vbox std_formatter
    let open_hvbox =
      pp_open_hvbox std_formatter
    let open_hovbox =
      pp_open_hovbox std_formatter
    let open_tbox =
      pp_open_tbox std_formatter
    let close_tbox =
      pp_close_tbox std_formatter
    let print_tbreak =
      pp_print_tbreak std_formatter
    let set_tab =
      pp_set_tab std_formatter
    let print_tab =
      pp_print_tab std_formatter
    let set_ellipsis_text =
      pp_set_ellipsis_text std_formatter
    let get_ellipsis_text =
      pp_get_ellipsis_text std_formatter
    let open_tag =
      pp_open_tag std_formatter
    let close_tag =
      pp_close_tag std_formatter
    let set_tags =
      pp_set_tags std_formatter
    let set_print_tags =
      pp_set_print_tags std_formatter
    let set_mark_tags =
      pp_set_mark_tags std_formatter
    let get_print_tags =
      pp_get_print_tags std_formatter
    let get_mark_tags =
      pp_get_mark_tags std_formatter
    let get_formatter_encoding =
      pp_get_formatter_encoding std_formatter
    let set_formatter_encoding =
      pp_set_formatter_encoding std_formatter
    let set_formatter_out_channel =
      pp_set_formatter_out_channel std_formatter
    let set_formatter_output_functions =
      pp_set_formatter_output_functions std_formatter
    let get_formatter_output_functions =
      pp_get_formatter_output_functions std_formatter
    let set_formatter_tag_functions =
      pp_set_formatter_tag_functions std_formatter
    let get_formatter_tag_functions =
      pp_get_formatter_tag_functions std_formatter
    let set_all_formatter_output_functions =
      pp_set_all_formatter_output_functions std_formatter
    let get_all_formatter_output_functions =
      pp_get_all_formatter_output_functions std_formatter

  end
end


module UBuffer =
struct
  type t = Camomile_buffer.buf
  let add_ustring = Camomile_buffer.add_string
  let add_buffer = Camomile_buffer.add_buffer
  let add_uchar = Camomile_buffer.add_char
  let add_string b s = Camomile_buffer.add_string b (UString.of_string s)
  let add_char b c = Camomile_buffer.add_char b (UChar.of_char c)
  let reset = Camomile_buffer.reset
  let clear = Camomile_buffer.clear
  let length = Camomile_buffer.length
  let contents b = Camomile_buffer.contents b
  let create = Camomile_buffer.create

  let sub b off len =
    Camomile_text.of_xstring (XString.sub b off len)

  let nth b off =
    XString.get b off

  let output_buffer c b =
    XString.iter (UChannel.output_uchar c) b

  let rec join_loop b s l =
    match l with
      | [] -> ()
      | h :: [] -> add_ustring b h
      | h :: (_ :: _) as t ->
	  (add_ustring b h; add_ustring b s; join_loop b s t)

  let join b sep l =
    join_loop b sep (List.rev l)

end

module UCollation =
struct
  include Camomile.UCol
end

module UStr =
struct
  include Camomile.UReStr
end

let u = UString.of_string
let u8= Encoding.decode Encoding.utf8
let escaped_uchar = Camomile_pervasives.escaped_uchar
