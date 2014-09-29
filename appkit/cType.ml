(* CType -- Basic types for C-stylish applications

Author: Michael Grünewald
Date: Wed May 22 08:08:04 CEST 2013

Copyright © 2013 Michael Grünewald

This file must be used under the terms of the CeCILL-B.
This source file is licensed as described in the file COPYING, which
you should have received as part of this distribution. The terms
are also available at
http://www.cecill.info/licences/Licence_CeCILL-B_V1-en.txt *)
open Printf

module Data =
struct
  type word = char
  type block = string
  type out_channel = Pervasives.out_channel
  let output_word = Pervasives.output_char
  let output_block = Pervasives.output
  let flush = Pervasives.flush
end

module Buffer =
struct
  type t = Buffer.t
  type word = Data.word
  type block = Data.block
  type out_channel = Data.out_channel
  let create = Buffer.create
  let add_word = Buffer.add_char
  let add_block = Buffer.add_string
  let contents = Buffer.contents
  let length = Buffer.length
  let output_buffer = Buffer.output_buffer
  let add_buffer = Buffer.add_buffer
  let clear = Buffer.clear
  let reset = Buffer.reset
  let nth = Buffer.nth
end

module Locale =
struct

  type t = {
    decimal_point : string;
    thousands_sep : string;
    grouping : int list;
    int_currency_symbol: string;
    int_currency_symbol_sep: string;
    loc_currency_symbol : string;
    mon_decimal_point : string;
    mon_thousands_sep : string;
    mon_grouping : int list;
    mon_positive_sign : string;
    mon_negative_sign : string;
    int_frac_digits : int;
    loc_frac_digits : int;
    p_cs_precedes: bool;
    n_cs_precedes : bool;
    p_sep_by_space : bool;
    n_sep_by_space : bool;
    p_sign_posn	: sign_posn;
    n_sign_posn	: sign_posn;
  } and sign_posn =
    | Parentheses
    | Before_everything
    | After_everything
    | Before_currency_symbol
    | After_currency_symbol

  (* We try to match as closely as possible the values defined by
  ISO/IEC~9899:TC2. (AKA C99, See 7.11)

  For this, we appropriately use 127 as an approximation for
  CHAR_MAX. We probably want to display less then 127 digits after
  point, though. *)
  let stdlocale = {
    decimal_point = ".";
    thousands_sep = "";
    grouping = [];
    int_currency_symbol = "";
    int_currency_symbol_sep = "";
    loc_currency_symbol = "";
    mon_decimal_point = "";
    mon_thousands_sep = "";
    mon_grouping = [];
    mon_positive_sign = "";
    mon_negative_sign = "";
    int_frac_digits = 127;
    loc_frac_digits = 127;
    p_cs_precedes = true;
    n_cs_precedes = true;
    p_sep_by_space = true;
    n_sep_by_space = true;
    p_sign_posn	= Before_everything;
    n_sign_posn	= Before_everything;
  }

end


module Value =
struct
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

  let is_of_kind :
  type a. a kind -> t -> bool =
    fun k1 (Thingie(k2,_)) -> match kind_eq k1 k2 with
    | Some(Witness) -> true
    | None -> false

  let is_of_same_kind (Thingie(k1,_)) (Thingie(k2,_)) =
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
    let is_plain_string s =
      String.length s = 0 || s.[0] <> '"'
    in
    fun k s -> match k, is_plain_string s with
    | String, true -> s
    | _ ->
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

  let kind_name :
  type a. a kind -> string =
    function
    | Bool -> "boolean"
    | Int -> "integer"
    | Char -> "character"
    | String -> "string"
    | Float -> "floating point"

  type locale = Locale.t

  type format = {
    specifier : char;
    left_justify : bool;
    always_signed : bool;
    align_sign : bool;
    alternative_format : bool;
    pad_with_zeroes : bool;
    width: int;
    precision: int;
  }

  let stdformat = {
    specifier = '\000';
    left_justify = false;
    always_signed = false;
    align_sign = false;
    alternative_format = false;
    pad_with_zeroes = false;
    width = 0;
    precision = -1;
  }

  let add_format_to_buffer b f =
    let add_char = Buffer.add_word b in
    let add_string = Buffer.add_block b in
    begin
      if f.left_justify then add_char '-';
      if f.always_signed then add_char '+';
      if f.align_sign && not f.always_signed then add_char ' ';
      if f.alternative_format then add_char '#';
      if f.pad_with_zeroes then add_char '0';
      if f.width > 0 then add_string (string_of_int f.width);
      if f.precision > -1 then (
	add_char '.'; add_string (string_of_int f.precision)
      );
      add_char f.specifier;
    end

  let format_to_string f =
    let b = Buffer.create 16 in (* 16 characters for our format *)
    begin
      add_format_to_buffer b f;
      Buffer.contents b
    end

  let invalid_format where f index =
    invalid_arg (String.concat ": " [
      "invalid format";
      where; f; string_of_int index
    ])

  let rec read_format s pos =
    let len = (String.length s) - pos in
    if len > 0 then
      read_flags s pos len stdformat 0
    else if len = 0 then
      stdformat
    else
      invalid_arg "read_format"

  and read_flags s pos len f i =
    if i = len then invalid_format "read_flags" s (pos + i);
    match s.[pos + i] with
    | '-' -> read_flags s pos len { f with left_justify = true; } (succ i)
    | '+' -> read_flags s pos len { f with always_signed = true; } (succ i)
    | ' ' -> read_flags s pos len { f with align_sign = true; } (succ i)
    | '#' -> read_flags s pos len { f with alternative_format = true; } (succ i)
    | '0' -> read_flags s pos len { f with pad_with_zeroes = true; } (succ i)
    |  _  -> read_width s pos len f i

  and read_integer where callback s pos len f i a =
    if i = len then invalid_format where s (pos + i);
    match s.[pos + i] with
    | '0'..'9' as d -> read_integer where callback s pos len f
      (succ i) (10 * a + (int_of_char d - (* int_of_char 0 *) 48))
    | _ -> callback s pos len f i a

  and read_width s pos len f i =
    read_integer "read_width" read_width_callback s pos len f i 0

  and read_width_callback s pos len f i a =
    read_precision s pos len { f with width = a; } i

  and read_precision s pos len f i =
    if i = len then invalid_format "read_precision" s (pos + i);
    match s.[pos + i] with
    | '.' -> read_integer "read_precision" read_precision_callback
      s pos len f (succ i) 0
    | _ -> read_specifier s pos len f i

  and read_precision_callback s pos len f i a =
    read_specifier s pos len { f with precision = a } i

  and read_specifier s pos len f i =
    if i = len then invalid_format "read_specifier" s (pos + i);
    { f with specifier = s.[pos + i]; }

  let format_of_string s =
    read_format s 0

  type buffer = Buffer.t

  (* Output contexts hold paramters common to our output procedures.
  Output procedures called add_* are leaf procedures finalising the
  context: the scratch buffer is transferred to the accumulator.
  Output procedures calleds prepare_* are intermediary procedures
  working on the scratch buffer.  Internal procedures adding
  characters to the accumulator are prefixed with commit, to emphasise
  their important character. *)

  type output_context = {
    c_locale : locale;
    c_format : format;
    c_accumulator : buffer;
    c_scratch : buffer;
  }

  (* We define many put_* procedures, putting (adding) characters to
  the scratch buffer of an output context.  There is also [rput_*]
  procedures doing the same but appending characters from right to
  left.

  The transfer and rtransfer procedure push the characters stacked in
  the scratch buffer to the actual buffer. *)

  let put_char ctxt c =
    Buffer.add_word ctxt.c_scratch c

  let put_string ctxt s =
    Buffer.add_block ctxt.c_scratch s

  (* Pad our scratch buffer with [a] characters [c]. *)
  let put_padding ctxt c a =
    for i = 1 to a do
      put_char ctxt c
    done

  let rput_padding =
    put_padding

  (* Compute the width (0 or 1) needed to display our sign. *)
  let sign_width fmt signflag =
    if signflag || fmt.always_signed || fmt.align_sign then
      1
    else
      0

  (* Compute the required padding size, reserving room for [reserved]
  characters. *)
  let padding_sz ctxt reserved =
    max 0 (
      ctxt.c_format.width
      - (Buffer.length ctxt.c_scratch)
      - reserved
    )

  (* Put padding for flushed right output.  The padding character
  is usually a 0. *)
  let rput_padding_for_flushedright ctxt c reserved =
    if not ctxt.c_format.left_justify then
      rput_padding ctxt c (padding_sz ctxt reserved)

  let rput_padding_and_sign ctxt signflag =
    let fmt = ctxt.c_format in
    let put = put_char ctxt in
    begin
      if fmt.pad_with_zeroes then
	rput_padding_for_flushedright ctxt '0' (
	  sign_width ctxt.c_format signflag
	);
      if signflag then
	put '-'
      else if fmt.always_signed then
	put '+'
      else if fmt.align_sign then
	put ' ';
      if not fmt.pad_with_zeroes then
	rput_padding_for_flushedright ctxt ' ' 0;
    end

  let commit_padding ctxt c a =
    for i = 1 to a do
      Buffer.add_word ctxt.c_accumulator c
    done

  let commit_scratch ctxt =
    Buffer.add_buffer ctxt.c_accumulator ctxt.c_scratch

  let rcommit_scratch ctxt =
    let len = Buffer.length ctxt.c_scratch in
    for i = 1 to len do
      Buffer.add_word ctxt.c_accumulator (
	Buffer.nth ctxt.c_scratch (len - i)
      )
    done

  let commit_generic reverseflag ctxt =
    let padding_sz =
      max 0 (ctxt.c_format.width - (Buffer.length ctxt.c_scratch))
    in
    commit_padding ctxt ' ' padding_sz;
    if reverseflag then (
      rcommit_scratch ctxt
    ) else (
      commit_scratch ctxt;
    )

  let commit =
    commit_generic false

  let rcommit =
    commit_generic true

  let finalise f ctxt x =
    begin
      f ctxt x;
      commit ctxt;
    end

  let rfinalise f ctxt x =
    begin
      f ctxt x;
      rcommit ctxt;
    end

  let add_number ctxt rput_digits signflag n =
    begin
      rput_digits ctxt n;
      rput_padding_and_sign ctxt signflag;
      rcommit ctxt;
    end

  let add_integer alt_prefix radix digits ctxt n =
    let rec rput_digits ctxt k =
      if k > 0 then
	begin
	  let q, r = k/radix, k mod radix (* r < 0 only if k < 0 *) in
	  Buffer.add_word ctxt.c_scratch digits.[r];
	  rput_digits ctxt q;
	end
      else
	()
    in
    begin
      if ctxt.c_format.alternative_format then
	put_string ctxt alt_prefix;
      add_number ctxt rput_digits (n < 0) (abs n);
    end

  let add_decimal =
    add_integer "" 10 "0123456789"

  let add_octal =
    add_integer "0o" 9 "01234567"

  let add_hexadecimal =
    add_integer "0x" 16 "0123456789abcdef"

  let add_hexadecimal_caps =
    add_integer "0X" 16 "0123456789ABCDEF"

  let add_char ctxt x =
    (* This is a leaf version of commit_char. *)
    Buffer.add_word ctxt.c_accumulator x

  let put_escaped_char ctxt c =
    (* We asssume ASCII *)
    let is_printable c =
      Char.code c >= 32 && Char.code c <= 126
    in
    let special_table c = match c with
      | '"' -> "\""
      | '\n' -> "\\n"
      | '\t' -> "\\t"
      | '\r' -> "\\r"
      | _ -> ""
    in
    let put_octal_char ctxt c =
      let x0 = c mod 8 in
      let c0 = (c - x0) / 8 in
      let x1 = c0 mod 8 in
      let x2 = (c0 - x1) / 8 in
      begin
	put_char ctxt '\\';
	put_char ctxt (Char.chr (48 + x2));
	put_char ctxt (Char.chr (48 + x1));
	put_char ctxt (Char.chr (48 + x0));
      end
    in
    match is_printable c, special_table c with
    | true, "" -> put_char ctxt c
    | false, _ -> put_octal_char ctxt (Char.code c)
    | _, s -> put_string ctxt s

  let add_escaped_char =
    finalise put_escaped_char

  let add_string ctxt v =
    Buffer.add_block ctxt.c_accumulator v

  let put_escaped_string ctxt v =
    String.iter (put_escaped_char ctxt) v

  let add_escaped_string =
    finalise put_escaped_string

  let add_bool ctxt v =
    add_string ctxt (if v then "true" else "false")

  (* Preparation of floating point representation is quite an involved
  task, so we rely on the OCaml-printf implementation to define an
  add_float_with_bprintf procedure.  An autonomous implementation is
  then provided, so the definition of add_float_with_bprintf could be
  removed once our code has been weel tested.  This does not take
  grouping information for our locale into account, so it is a broken
  solution. Also note the—not that nice—relying on Obj.magic to cast
  the printf format. *)

  let buffer_finalise b =
    let answer = Buffer.contents b in
    begin
      Buffer.clear b;
      answer
    end

  let make_float_format buffer format :
      (float -> 'a, 'b, 'c, 'd, 'd, 'a) format6 =
    begin
      Buffer.add_word buffer '%';
      add_format_to_buffer buffer format;
      Obj.magic (buffer_finalise buffer)
    end

  let put_float_with_bprintf ctxt v =
    let printf_format =
      make_float_format ctxt.c_scratch ctxt.c_format
    in
    Printf.bprintf ctxt.c_scratch "%(%f%)" printf_format v

  let add_float_with_bprintf =
    finalise put_float_with_bprintf

  let string_of_float_with_bprintf ctxt v =
    let printf_format =
      make_float_format ctxt.c_scratch { ctxt.c_format with width = 0; }
    in
    begin
      Printf.bprintf ctxt.c_scratch "%(%f%)" printf_format v;
      buffer_finalise ctxt.c_scratch
    end


  (* Quote from lib/libc/stdio/vfprintf.c (FreeBSD) :

    We can decompose the printed representation of floating
    point numbers into several parts, some of which may be empty:

    [+|-| ] [0x|0X] MMM . NNN [e|E|p|P] [+|-] ZZ
       A       B     ---C---      D       E   F

    A:  'sign' holds this value if present; '\0' otherwise
    B:  ox[1] holds the 'x' or 'X'; '\0' if not hexadecimal
    C:  cp points to the string MMMNNN.  Leading and trailing
        zeros are not in the string and must be added.
    D:  expchar holds this character; '\0' if no exponent, e.g. %f
    F:  at least two digits for decimal, at least one digit for hex

  We use this quote to define our description of what is a
  representation for floats. *)

  type float_representation = {
    f_repr: string;   (* representation acquired with string_of_float *)
    f_repr_len: int;  (* representation length *)
    f_signflag: bool; (* true iff negative *)
    f_expn_len: int;  (* length of exponent, up to the e, 0 if absent *)
    f_frac_len: int;  (* length of fraction part up to the dot, 0 if absent *)
    f_intg_len: int;  (* length of the integral part, 0 if absent *)
  }

  let make_float_representation ctxt f =
    let repr = string_of_float_with_bprintf ctxt f in
    let repr_len = String.length repr in
    let signflag = (repr_len > 0) && (repr.[0] = '-') in
    let expn_len =
      try repr_len - String.rindex repr 'e'
      with Not_found -> 0
    in
    let frac_len =
      try repr_len - expn_len - String.rindex repr '.'
      with Not_found -> 0
    in
    let intg_len =
      repr_len - expn_len - frac_len - (if signflag then 1 else 0)
    in {
      f_repr = repr;
      f_repr_len = repr_len;
      f_signflag = signflag;
      f_expn_len = expn_len;
      f_frac_len = frac_len;
      f_intg_len = intg_len;
    }

  (* Because of the definition of our grouping information,
  it is easier to put *backwards* our representation. *)

  let put_float monetary ctxt v =
    let open Locale in
    let fmt = ctxt.c_format in
    let locale = ctxt.c_locale in
    let repr = make_float_representation ctxt v in
    let decimal_point =
      if monetary
      then
	locale.mon_decimal_point
      else
	locale.decimal_point
    in
    let thousands_sep =
      if monetary
      then
	locale.mon_thousands_sep
      else
	locale.thousands_sep
    in
    let grouping =
      if monetary
      then
	locale.mon_grouping
      else
      locale.grouping
    in
    let put_char c =
	Buffer.add_word ctxt.c_scratch c
    in
    let put_substring s off len =
      for i = len - 1 downto 0 do
	put_char s.[off + i]
      done
    in
    let put_string s =
      put_substring s 0 (String.length s)
    in
    (* unused
    let put_subrepr r off len =
      put_substring r.f_repr off len
    in
    *)
    let put_rsubstring s off len =
      for i = 1 to len  do
	put_char s.[off - i]
      done
    in
    let put_rsubrepr r off len =
      put_rsubstring r.f_repr off len
    in
    let put_exponent r =
      put_rsubrepr repr repr.f_repr_len r.f_expn_len
    in
    let frac_digit_n = (* # digits available in frac part *)
      max 0 (repr.f_frac_len - 1)
    in
    let frac_off = (* offset of frac part *)
      repr.f_repr_len - repr.f_expn_len - repr.f_frac_len
    in
    let frac_len = (* # frac digits we are willing to print *)
      min fmt.precision frac_digit_n
    in
    let pad_sz = (* # of padding digits *)
      fmt.precision - frac_len
    in
    let put_grouped_digits r grouping =
      (* offset is the offset of the start of our integral part
         len is the length of our integral part
         l is the list of groups
      *)
      let put_digit i =
	put_char r.f_repr.[frac_off - 1 - i]
      in
      let put_separator () =
	put_string thousands_sep
      in
      let rec loop i j l =
      (* i is the number of digits processed so far
         j is the number of digits prcessed in the current group
      *)
	match l with
	| []
	| -1 :: _
	|  0 :: _ -> (put_digit i; maybe_loop (succ i) j l)
	|  h :: t when j < h -> (put_digit i; maybe_loop (succ i) (succ j) l)
	|  h :: [] (* when j >= h *) -> (put_separator(); loop i 0 l)
	|  h :: t  (* when j >= h *) -> (put_separator(); loop i 0 t)
      and maybe_loop i j l =
	if i < r.f_intg_len
	then
	  loop i j l
      in
      loop 0 0 grouping
    in
    let maybe_pad_and_put_sign () =
      if fmt.pad_with_zeroes then
	rput_padding_for_flushedright ctxt '0'
	  (sign_width ctxt.c_format repr.f_signflag);
      if repr.f_signflag then
	put_char '-'
    in
    begin
      put_exponent repr;
      put_padding ctxt '0' pad_sz;
      if frac_len > 1 then
	put_rsubrepr repr (frac_off + frac_len + 1) frac_len;
      if fmt.precision > 0 then put_string decimal_point;
      put_grouped_digits repr grouping;
      maybe_pad_and_put_sign ();
    end

  let add_float monetary ctxt v =
    begin
      put_float monetary ctxt v;
      rcommit ctxt;
    end

  let add_formatted scratch locale ax format (Thingie(kind,value)) =
    let ctxt = {
      c_locale = locale;
      c_format = format;
      c_accumulator = ax;
      c_scratch = scratch;
    }
    in
    let internal :
      type a. a kind -> a -> unit = fun kind value ->
    match format.specifier, kind, value with
    | 'S', String, x -> add_escaped_string ctxt x
    | 's', String, x -> add_string ctxt x
    | 'B', Bool, x -> add_bool ctxt x
    | 'd', Int, x -> add_decimal ctxt x
    | 'i', Int, x -> add_decimal ctxt x
    | 'n', Int, x -> add_decimal ctxt x
    | 'o', Int, x -> add_octal ctxt x
    | 'x', Int, x -> add_hexadecimal ctxt x
    | 'X', Int, x -> add_hexadecimal_caps ctxt x
    | 'c', Char, x -> add_char ctxt x
    | 'C', Char, x -> add_escaped_char ctxt x
    | 'f', Float, x -> add_float false ctxt x
    | 'F', Float, x -> add_float false ctxt x
    | 'e', Float, x -> add_float false ctxt x
    | 'E', Float, x -> add_float false ctxt x
    | 'g', Float, x -> add_float false ctxt x
    | 'G', Float, x -> add_float false ctxt x
    | _ -> invalid_arg "add_formatted"
    in
    internal kind value

  let add_text _ _ ax text =
    Buffer.add_block ax text

  let add scratch l ax v =
    add_formatted scratch l ax stdformat v
end

module Database =
struct
  type t = unit
  type connection_token = unit
  let opendb () = ()
  let find () id = id
  let close () = ()
end


module Classification =
struct

  type t =
  | Debug
  | Info
  | Notice
  | Warning
  | Error
  | Critical
  | Alert
  | Emergency

  type locale = Locale.t

  let compare = Pervasives.compare

  let decoration = function
    | Debug -> "Debug: "
    | Info -> "Info: "
    | Notice -> "Notice: "
    | Warning -> "Warning: "
    | Error -> "Error: "
    | Critical -> "Critical: "
    | Alert -> "Alert: "
    | Emergency -> "Emergency: "

  type buffer = Buffer.t

  let add_pre l b cls id =
    Buffer.add_block b (decoration cls)

  let add_post _ _ _ _ = ()

  type out_channel = Pervasives.out_channel
  let send_pre _ _ _ _ = ()
  let send_post _ _ _ _ = ()

  let to_string c =
    match c with
    | Debug -> "debug"
    | Info -> "info"
    | Notice -> "notice"
    | Warning -> "warning"
    | Error -> "error"
    | Critical -> "critical"
    | Alert -> "alert"
    | Emergency -> "emergency"

  let of_string s =
    match s with
    | "debug" -> Debug
    | "info" -> Info
    | "notice" -> Notice
    | "warning" -> Warning
    | "error" -> Error
    | "critical" -> Critical
    | "alert" -> Alert
    | "emergency" -> Emergency
    | _ -> invalid_arg (sprintf "unknown message classification: %S" s)
end
