(* Getopt -- Program arguments analysis

Author: Michael Grünewald
Date: Sun  4 May 2008 11:08:10 CEST

Copyright © 2008-2014 Michael Grünewald

This file must be used under the terms of the CeCILL-B.
This source file is licensed as described in the file COPYING, which
you should have received as part of this distribution. The terms
are also available at
http://www.cecill.info/licences/Licence_CeCILL-B_V1-en.txt *)
open Printf

let buffer_sz = 100		(* Size of buffers holding argument options *)

let progname () =
  Filename.basename Sys.executable_name

exception Error of string

(* Error messages *)

let error fmt =
  ksprintf (fun s -> raise(Error(s))) fmt

let error_illegal_short c =
  error "illegal option -- %s" (Char.escaped c)

let error_illegal_long c opt =
  error "illegal option -- %s %S" (Char.escaped c) opt

let error_argument_short c =
  error "option requires an argument -- %s" (Char.escaped c)

let error_argument_long c name =
  error "option requires an argument -- %s %s" (Char.escaped c) name

let error_separator () =
  error "illegal double dash construction"

let error_convert_short c opt mesg =
  error "invalid argument -- %s %S: %s" (Char.escaped c) opt mesg

let error_convert_long c name opt mesg =
  error "invalid argument -- %s %s %S: %s" (Char.escaped c) name opt mesg


(* Analyse of the command line *)

type token =
| Flag of char
| Option of char * string
| Rest of string

(* A word stream is a list of character options, representing words
separated by [None] .*)

(* Convert a vector of string to a wordstream. *)
let wordstream_of_argv argv =
  let string_to_list s =
    Array.init (String.length s) (String.get s)
      |> Array.to_list
  in
  let rec flatten wordlist =
    let embed lst =
      List.map (fun x -> Some(x)) lst
    in
    match wordlist with
    | w1 :: w2 :: t -> (embed w1)@(None::flatten(w2::t))
    | w :: [] -> embed w
    | [] -> []
  in
  let streamdata =
    Array.to_list argv
      |> List.map string_to_list
      |> flatten
  in
  Stream.of_list streamdata

(* Read the stream until the next gap represented by [None]
   or the end of the stream. *)
let wordstream_readword buffer stream =
  begin
    (try
       while true do match Stream.next stream with
       | Some c -> Buffer.add_char buffer c
       | None -> raise Exit
       done
     with
     | Exit -> ()
     | Stream.Failure -> ());
    let answer = Buffer.contents buffer in
    begin
	Buffer.clear buffer;
	answer
    end
  end

let wordstream_is_empty stream =
  Stream.peek stream = None

let argv_parser flags options argv =
  let stream = wordstream_of_argv argv in
  let buffer = Buffer.create buffer_sz in
  let answer = ref [] in
  let is_flag c = String.contains flags c in
  let is_option c = String.contains options c in
  let pack_flag c = answer := !answer @ [Flag(c)] in
  let pack_option c arg = answer := !answer @ [Option(c,arg)] in
  let pack_rest s =  answer := !answer @ [Rest(s)] in
  let slurp () = wordstream_readword buffer stream in
  let rec maybe_read_cluster () =
    match Stream.next stream with
    | Some('-') -> read_cluster_enter ()
    | Some(c) -> (Buffer.add_char buffer c; read_trail())
    | None -> maybe_read_cluster ()
  and read_trail () =
    (pack_rest(slurp()); read_rest ())
  and read_rest () =
    if wordstream_is_empty stream then () else read_trail ()
  and read_cluster_enter () =
    match Stream.next stream with
    | None -> (pack_rest "-"; read_rest ())
    | Some('-') -> read_end_of_separator ()
    | Some(c) -> read_cluster_option c
  and read_cluster_continue () =
    match Stream.next stream with
    | None -> maybe_read_cluster ()
    | Some('-') -> (pack_rest "-"; read_rest())
    | Some(c) -> read_cluster_option c
  and read_cluster_option c =
    if is_flag c then
      (pack_flag c; read_cluster_continue())
    else if is_option c then
      read_option_argument c
    else
      error_illegal_short c
  and read_option_argument c =
    if wordstream_is_empty stream then
      error_argument_short c
    else begin
      if Stream.peek stream = Some(None) then Stream.junk stream;
      (pack_option c (slurp()); maybe_read_cluster())
    end
  and read_end_of_separator () =
    if Stream.peek stream = Some(None) then
      (Stream.junk stream; read_rest ())
    else
      error_separator ()
  in
  ( try maybe_read_cluster () with Stream.Failure -> ());
  !answer


(* Traditional flags and options *)

type t = {
  option: char;
  help: string;
  callback: string -> unit;
  wants_arg: bool;
}

type note = {
  title: string;
  content: string;
}

type spec = {
  usage: string;
  description: string;
  options: t list;
  rest: string -> unit;
  notes: note list;
}

type 'a reader =
  string -> 'a

let supervise_convert_short c reader s =
  try reader s
  with
  | Invalid_argument(mesg)
  | Failure(mesg) -> error_convert_short c s mesg
  | _ -> failwith "Getopt.supervise_convert_short"
  (* It is not permitted to reader to throw something other than a
     failure or an invalid argument. *)

let flag c cb descr = {
  option = c;
  help = descr;
  callback = (fun _ ->  cb ());
  wants_arg = false;
}

let concrete reader c cb descr = {
  option = c;
  help = descr;
  callback = (fun s -> cb (supervise_convert_short c reader s));
  wants_arg = true;
}

let char_of_string s =
  if String.length s != 1 then
    failwith "char_of_string"
  else
    s.[0]

let char =
  concrete char_of_string

let bool =
  concrete bool_of_string

let string =
  concrete (function x -> x)

let int =
  concrete int_of_string

let float =
  concrete float_of_string

let note title content = {
  title;
  content;
}

let spec usage description options rest notes = {
  usage;
  description;
  options;
  rest;
  notes;
}


(* Preparation of help messages *)

module Message =
struct
  open Format

  let alinea_open title =
    print_cut ();
    open_hvbox 1;
    print_string title;
    print_char ':';
    print_space ()

  let alinea_close () =
    close_box ()

  let paragraph_open () =
    open_box 0

  let paragraph_close () =
    close_box ()

  let paragraph_switch () =
    paragraph_close ();
    print_cut ();
    paragraph_open ()

  let paragraph_insert text =
    let l = String.length text in
    for i = 0 to l - 1 do
      match text.[i] with
      | '\n' -> paragraph_switch ()
      | ' ' -> print_space ()
      | c -> print_char c
    done

  let compose_note note =
    alinea_open note.title;
    paragraph_open ();
    paragraph_insert note.content;
    paragraph_close ();
    alinea_close ()

  let usage_open () =
    open_hovbox 1;
    print_string "Usage:";
    print_space ()

  let usage_close () =
    close_box ();
    print_cut ()

  let usage_open_bracket () =
    print_cut();
    open_hbox ();
    print_char '['

  let usage_close_bracket () =
    print_char ']';
    close_box ();
    print_cut ()

  let usage_insert text =
    let l = String.length text in
    for i = 0 to l - 1 do
      match text.[i] with
      | '\n' -> failwith "usage_insert_text: newlines are not allowed here"
      | ' ' -> print_space ()
      | '[' -> usage_open_bracket ()
      | ']' -> usage_close_bracket ()
      | c -> print_char c
      done

  let compose_usage usage description =
    usage_open ();
    usage_insert usage;
    usage_close ();
    open_hbox ();
    print_char ' ';
    print_string description;
    close_box ();
    print_cut ()

  let option_open option =
    print_cut();
    open_vbox 3;
    print_char '-';
    print_char option;
    print_char ' '

  let option_close () =
    close_box()

  let option_newline () =
    close_box ();
    print_cut ();
    open_box 0

  let option_insert text =
    let l = String.length text in
    for i = 0 to l - 1 do
      match text.[i] with
      | '\n' -> option_newline ()
      | ' ' -> print_space ()
      | c -> print_char c
    done

  let compose_option option =
    option_open option.option;
    open_box 0;
    option_insert option.help;
    close_box ();
    option_close()

  (* Prepare the help message associated to the given spec. *)
  let help spec =
    open_vbox 0;
    compose_usage (progname () ^ " " ^ spec.usage) spec.description;
    open_vbox 1;
    print_string "Options:";
    List.iter compose_option spec.options;
    close_box();
    List.iter compose_note spec.notes;
    print_cut();
    close_box()

  (* Prepare the usage message associated to the given spec. *)
  let usage spec =
    Format.set_formatter_out_channel stderr;
    open_vbox 0;
    usage_open ();
    usage_insert (progname () ^ " " ^ spec.usage);
    usage_close ();
    close_box()
end


module OptionList =
struct

  let has_option c x =
    x.option = c

  let is_option lst c =
    List.exists (has_option c) lst

  let wants_arg lst c =
    (List.find (has_option c) lst).wants_arg

  let callback lst c =
    (List.find (has_option c) lst).callback

  let set predicate lst =
    let buf = Buffer.create buffer_sz in
    List.filter predicate lst
      |> List.map (fun x -> x.option)
      |> List.iter (Buffer.add_char buf);
    Buffer.contents buf

  let flags lst =
    set (fun x -> not(x.wants_arg)) lst

  let options lst =
    set (fun x -> x.wants_arg) lst
end

let help_callback =
  ref (fun () -> failwith "Getopt.help_callback")

let help_flag = {
  option = 'h';
  wants_arg = false;
  help = "Display available options.";
  callback = fun _ -> !help_callback ();
}

let maybe_add_help spec =
  if OptionList.is_option spec.options 'h'
  then spec else { spec with options = help_flag :: spec.options }

let help spec =
  Message.help spec;
  exit 0

let usage spec mesg =
  eprintf "%s: %s\n" (progname()) mesg;
  Message.usage spec;
  exit 64 (* See sysexits(3) *)

let apply spec tok =
  match tok with
  | Flag(c) -> (OptionList.callback spec.options c) ""
  | Option(c, optarg) -> (OptionList.callback spec.options c) optarg
  | Rest(s) -> spec.rest s

let parse spec0 argv =
  let spec = maybe_add_help spec0 in
  let flags = OptionList.flags spec.options in
  let options = OptionList.options spec.options in
  try
    help_callback := (fun () -> help spec);
    List.iter (apply spec) (argv_parser flags options argv)
  with
  | Error(mesg) -> usage spec mesg

let parse_argv spec =
  let argv = Array.sub Sys.argv 1 (Array.length Sys.argv - 1) in
  parse spec argv


(* Long options *)

type long_option = {
  long_option: string;
  long_callback: string -> unit;
  long_wants_arg: bool;
}

let supervise_convert_long name reader s =
  try reader s
  with
  | Invalid_argument(mesg)
  | Failure(mesg) -> error_convert_long '?' name mesg s
  | _ -> failwith "Getopt.supervise_convert_long"
  (* It is not permitted to reader to throw something other than a
     failure or an invalid argument. *)

let long_flag name cb = {
  long_option = name;
  long_callback = (function _ -> cb ());
  long_wants_arg = false;
}

let long_concrete reader name cb = {
  long_option = name;
  long_callback = (fun s -> cb (supervise_convert_long name reader s));
  long_wants_arg = true;
}

let long_char =
  long_concrete char_of_string

let long_bool =
  long_concrete bool_of_string

let long_string =
  long_concrete (fun x -> x)

let long_int =
  long_concrete int_of_string

let long_float =
  long_concrete float_of_string

let long_option_get_name s =
  try String.sub s 0 (String.index s '=')
  with Not_found -> s

let long_option_has_name n x =
  n = x.long_option

let long_option_callback o f s =
  let l = String.length s in
  let n = long_option_get_name s in
  let m =
    try List.find (long_option_has_name n) f
    with Not_found -> error_illegal_long o n
  in
  if m.long_wants_arg then
    let i =
      try String.index s '='
      with Not_found -> error_argument_long o n
    in
    let a = String.sub s (i+1) (l-i-1) in
    try m.long_callback a
    with Error(mesg) -> (mesg.[String.index mesg '?'] <- o; raise(Error(mesg)))
  else
    m.long_callback ""

let long o c d = {
    option = o;
    callback = long_option_callback o c;
    help = d;
    wants_arg = true;
}


(* Definition of callbacks *)

let store r v =
  r := v

let set v r () =
  r := v

let queue a v =
  a := !a @ [v]
