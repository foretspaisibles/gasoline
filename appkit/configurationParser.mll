{
open Printf

type error =
| Illegal_character
| Illegal_escape_sequence
| Unterminated_string
| Expecting_binding
| Expecting_value
| Expecting_path_elt
| Expecting_path_sep_or_term


let error_to_string = function
| Illegal_character -> "illegal character"
| Illegal_escape_sequence -> "illegal escape sequence"
| Unterminated_string -> "unterminated string"
| Expecting_binding -> "expecting binding"
| Expecting_value -> "expecting value"
| Expecting_path_elt -> "expecting path element"
| Expecting_path_sep_or_term -> "expecting path separator or terminator"


exception Error of error * string * Lexing.position


let error code lexbuf =
  raise(Error(code, Lexing.lexeme lexbuf, lexbuf.Lexing.lex_curr_p))


type token =
| SECTION_BEGIN
| SECTION_ELT of string
| SECTION_SEP
| SECTION_END
| IDENT of string
| EQUAL
| VALUE of string
| COMMENT of string
| EOF

module Context =
struct
  let buffer_sz = 512

  type mode =
  | MNORMAL
  | MSECTION
  | MVALUE

  type t = {
    mutable m: mode;
    mutable p: Lexing.position;
    b: Buffer.t;
    fname: string;
  }

  let make fname = {
    m = MNORMAL;
    p = Lexing.dummy_pos;
    b = Buffer.create buffer_sz;
    fname;
  }

  let enter_normal_mode f c lexbuf =
    let a = f c lexbuf in
    begin
      c.m <- MNORMAL;
      Buffer.clear c.b;
      a
    end

  let enter_section_mode f c lexbuf =
    let a = f c lexbuf in
    begin
      c.m <- MSECTION;
      a
    end

  let enter_value_mode f c lexbuf =
    let a = f c lexbuf in
    begin
      Buffer.clear c.b;
      c.m <- MVALUE;
      c.p <- lexbuf.Lexing.lex_curr_p;
      a
    end

  let char_of_escaped c = match c with
    | 'n' -> '\n'
    | 't' -> '\t'
    | 'b' -> '\b'
    | 'r' -> '\r'
    |  u -> u

  let char_of_decimal_code lexbuf off =
    let int_of_digit i =
      Char.code(Lexing.lexeme_char lexbuf (off + i)) - 48
    in
    let code =
      100 * (int_of_digit 0) +
       10 * (int_of_digit 1) +
        1 * (int_of_digit 2)
    in
    begin
      if (code < 0 || code > 255) then
	error Illegal_escape_sequence lexbuf
      else
	Char.chr code
    end

  let char_of_hexadecimal_code lexbuf off =
    let int_of_digit i =
      let digit = Char.code (Lexing.lexeme_char lexbuf (off + i)) in
      if digit >= 97
      then
	digit - 87
      else if digit >= 65
      then
	digit - 55
      else
	digit - 48
    in
    let code =
      16 * (int_of_digit 0) +
       1 * (int_of_digit 1)
    in
    begin
      if (code < 0 || code > 255) then
	error Illegal_escape_sequence lexbuf
      else
	Char.chr code
    end

  let store_char c u =
    Buffer.add_char c.b u

  let store_current_char c lexbuf =
    Buffer.add_char c.b (Lexing.lexeme_char lexbuf 0)

  let store_escaped_char c lexbuf =
    store_char c (char_of_escaped (Lexing.lexeme_char lexbuf 1))

  let store_decimal_code c lexbuf =
    store_char c (char_of_decimal_code lexbuf 1)

  let store_hexadecimal_code c lexbuf =
    store_char c (char_of_hexadecimal_code lexbuf 2)

  let contents c =
    Buffer.contents c.b

  let with_fname c pos =
    { pos with Lexing.pos_fname = c.fname }

  let startpos c =
    with_fname c c.p

  let is_empty c =
    Buffer.length c.b = 0

end

module Token =
struct
  type t =
    token

  let to_string tok = match tok with
    | SECTION_BEGIN -> "SECTION_BEGIN"
    | SECTION_ELT s -> sprintf "SECTION_ELT(%S)" s
    | SECTION_SEP -> "SECTION_SEP"
    | SECTION_END -> "SECTION_END"
    | IDENT s -> sprintf "IDENT(%S)" s
    | EQUAL -> "EQUAL"
    | VALUE s -> sprintf "VALUE(%S)" s
    | COMMENT s -> sprintf "COMMENT(%S)" s
    | EOF -> "EOF"

end

module Position =
struct
  type t =
    Lexing.position

  let to_repr pos =
    sprintf "position { %S; %d; %d; %d }"
      pos.Lexing.pos_fname
      pos.Lexing.pos_lnum
      pos.Lexing.pos_bol
      pos.Lexing.pos_cnum

  let to_linecol pos =
    (
      pos.Lexing.pos_lnum,
      pos.Lexing.pos_cnum - pos.Lexing.pos_bol
    )

  let to_string pos =
    let (line_no, char_no) = to_linecol pos in
    sprintf "line %d, character %d" line_no char_no

  let make fname =
    {
      Lexing.
      pos_fname = fname;
      pos_lnum = 1;
      pos_bol = 0;
      pos_cnum = 0;
    }
end

module PToken =
struct

  type t =
    Position.t * Position.t * Token.t

  let startpos (p,_,_) = p

  let endpos (_,p,_) = p

  let token (_,_,tok) = tok

  let to_string (startpos, endpos, tok) =
    let (startline, startcol) = Position.to_linecol startpos in
    let (endline, endcol) = Position.to_linecol endpos in
    sprintf "%03d:%02d-%03d:%02d: %s"
      startline startcol
      endline endcol
      (Token.to_string tok)

  let newline lexbuf a =
    Lexing.new_line lexbuf;
    a

  let raw c lexbuf =
    (
      Context.with_fname c lexbuf.Lexing.lex_start_p,
      Context.with_fname c lexbuf.Lexing.lex_curr_p,
      (Lexing.lexeme lexbuf)
    )

  let cook f c lexbuf =
    let (startpos, endpos, tok) = raw c lexbuf in
    (startpos, endpos, f tok)

  let fixed token c lexbuf =
    (
      Context.with_fname c lexbuf.Lexing.lex_start_p,
      Context.with_fname c lexbuf.Lexing.lex_curr_p,
      token
    )

  let new_line f c lexbuf =
    let a = f c lexbuf in
    begin
      Lexing.new_line lexbuf;
      a
    end

  let section_begin =
    Context.enter_section_mode (fixed SECTION_BEGIN)

  let section_elt =
    cook (fun s -> SECTION_ELT s)

  let section_sep =
    fixed SECTION_SEP

  let section_end =
    Context.enter_normal_mode (fixed SECTION_END)

  let ident =
    cook (fun s -> IDENT s)

  let equal =
    Context.enter_value_mode (fixed EQUAL)

  let value =
    let make c lexbuf =
      (
	Context.startpos c,
	lexbuf.Lexing.lex_curr_p,
	VALUE(Context.contents c)
      ) in
    new_line(Context.enter_normal_mode make)

  let comment =
    cook (fun s -> COMMENT s)

  let eof =
    fixed EOF

  let quoted_section_elt c lexbuf =
    (
      Context.startpos c,
      lexbuf.Lexing.lex_curr_p,
      SECTION_ELT(Context.contents c)
    )

  let quoted_value =
    let make c lexbuf =
      (
	Context.startpos c,
	lexbuf.Lexing.lex_curr_p,
	VALUE(Context.contents c)
      ) in
    Context.enter_normal_mode make

end

}

let wsp = [' ' '\t']
let quoted = "\"[^\"*]\""
let char_alpha = [ 'A' - 'Z' 'a' - 'z' ]
let char_digit = [ '0' - '9' ]
let char_special_ident = [ '-' '_' '*' '.' '?' ]
let char_special_section = [ '-' '_' '*' '?' ]
let ident_symbol =
  ( char_special_ident | char_alpha )
  ( char_special_ident | char_alpha | char_digit ) *
let section_symbol =
  ( char_special_section | char_alpha )
  ( char_special_section | char_alpha | char_digit ) *

let ident = quoted | ident_symbol
let section_elt = quoted | section_symbol

let section_begin = '['
let section_sep = '.'
let section_end = ']'

let equal = '='

rule normal c = parse
  | wsp
      { normal c lexbuf }
  | '\n'
      { Lexing.new_line lexbuf;
	normal c lexbuf }
  | '#'
      { comment c lexbuf }
  | section_begin
      { PToken.section_begin c lexbuf }
  | ident
      { PToken.ident c lexbuf }
  | equal wsp*
      { PToken.equal c lexbuf }
  | eof
      { PToken.eof c lexbuf }
and comment c = parse
  | [ ^ '\n' ]*
      { PToken.comment c lexbuf }
and section c = parse
  | wsp
      { section c lexbuf }
  | '"'
      { quoted_string PToken.quoted_section_elt c lexbuf }
  | section_sep
      { PToken.section_sep c lexbuf }
  | section_elt
      { PToken.section_elt c lexbuf }
  | section_end
      { PToken.section_end c lexbuf }
  | eof
      { PToken.eof c lexbuf }
and value c = parse
  | '"'
      { if Context.is_empty c then
	  quoted_string PToken.quoted_value c lexbuf
	else begin
	  Context.store_current_char c lexbuf;
          value c lexbuf
	end }
  | '\\' '\n'
      { Context.store_char c '\n';
	Lexing.new_line lexbuf;
        value c lexbuf }
  | '\n'
      { PToken.value c lexbuf }
  | eof
      { PToken.value c lexbuf }
  | _
      { Context.store_current_char c lexbuf;
        value c lexbuf }
and quoted_string finalizer c = parse
  | '"'
      { finalizer c lexbuf }
  | '\\' '\n' wsp *
      { quoted_string finalizer c lexbuf }
  | '\\' ['\\' '\'' '"' 'n' 't' 'b' 'r' ' ']
      { Context.store_escaped_char c lexbuf;
	quoted_string finalizer c lexbuf }
  | '\\' ['0'-'9'] ['0'-'9'] ['0'-'9']
      { Context.store_decimal_code c lexbuf;
	quoted_string finalizer c lexbuf }
  | '\\' 'x' ['0'-'9' 'a'-'f' 'A'-'F'] ['0'-'9' 'a'-'f' 'A'-'F']
      { Context.store_hexadecimal_code c lexbuf;
	quoted_string finalizer c lexbuf }
  | '\n'
      { Context.store_char c '\n';
	Lexing.new_line lexbuf;
	quoted_string finalizer c lexbuf }
  | eof
      { error Unterminated_string lexbuf }
  | _
      { Context.store_current_char c lexbuf;
	quoted_string finalizer c lexbuf }


{
let token c lexbuf =
  let open Context in
  match c.m with
  | MNORMAL -> normal c lexbuf
  | MSECTION -> section c lexbuf
  | MVALUE -> value c lexbuf


let rec process_lexbuf c lexbuf =
  let tok = token c lexbuf in
  print_endline (PToken.to_string tok);
  if PToken.token tok = EOF then
    ()
  else
    process_lexbuf c lexbuf

let process_file file =
  let c = open_in file in
  let l = Lexing.from_channel c in
  let u = Context.make file in
  try
    process_lexbuf u l;
    close_in c;
  with exn -> close_in c; raise exn


type pos =
  Lexing.position

type excerpt =
  Lexing.position * Lexing.position * string

let startpos (pos,_,_) =
  pos

let endpos (_,pos,_) =
  pos

let text (_,_,tok) =
  tok

module type Definition =
sig
  type t
  val comment : t -> excerpt -> unit
  val section : t -> excerpt list -> unit
  val binding : t -> excerpt -> excerpt -> unit
  val parse_error : t -> pos -> error -> unit
end

module type S =
sig
  type t
  val parse : t -> Lexing.lexbuf -> unit
  val parse_string : t -> string -> unit
  val parse_channel : t -> in_channel -> unit
  val parse_file : t -> string -> unit
end

module Make(D:Definition) =
struct

  type t =
    D.t

  type state = {
    c: Context.t;
    d: D.t;
  }

  let rec parse_lexbuf p lexbuf =
    let (startpos, endpos, tok) = token p.c lexbuf in
    match tok with
    | SECTION_BEGIN -> parse_section_elt p lexbuf []
    | IDENT s -> parse_binding p lexbuf (startpos, endpos, s)
    | COMMENT s -> ( D.comment p.d (startpos, endpos, s);
		     parse_lexbuf p lexbuf )
    | EOF -> ()
    | _ -> D.parse_error p.d startpos Illegal_character
  and parse_binding p lexbuf key =
    let (startpos, endpos, tok) = token p.c lexbuf in
    match tok with
    | EQUAL -> parse_value p lexbuf key
    | _ -> D.parse_error p.d startpos Expecting_binding
  and parse_value p lexbuf key =
    let (startpos, endpos, tok) = token p.c lexbuf in
    match tok with
    | VALUE s -> ( D.binding p.d key (startpos, endpos, s);
		   parse_lexbuf p lexbuf )
    | _ -> D.parse_error p.d startpos Expecting_value
  and parse_section_elt p lexbuf ax =
    let (startpos, endpos, tok) = token p.c lexbuf in
    match tok with
    | SECTION_ELT elt -> parse_section p lexbuf ((startpos, endpos, elt)::ax)
    | _ -> D.parse_error p.d startpos Expecting_path_elt
  and parse_section p lexbuf ax =
    let (startpos, endpos, tok) = token p.c lexbuf in
    match tok with
    | SECTION_SEP -> parse_section_elt p lexbuf ax
    | SECTION_END -> ( D.section p.d (List.rev ax);
		       parse_lexbuf p lexbuf )
    | _ -> D.parse_error p.d startpos Expecting_path_sep_or_term

  let safe_parse_lexbuf fname d lexbuf =
    try parse_lexbuf {
      c = Context.make fname;
      d = d;
    } lexbuf
    with Failure "lexing: empty token" -> (
      D.parse_error d lexbuf.Lexing.lex_curr_p Illegal_character
    )

  let parse d lexbuf =
    safe_parse_lexbuf "" d lexbuf

  let parse_channel d channel =
    safe_parse_lexbuf "" d (Lexing.from_channel channel)

  let parse_file d fname =
    let channel = open_in fname in
    try
      safe_parse_lexbuf fname d (Lexing.from_channel channel);
      close_in channel;
    with exn -> close_in channel; raise exn

  let parse_string d s =
    safe_parse_lexbuf "" d (Lexing.from_string s)

end
}
