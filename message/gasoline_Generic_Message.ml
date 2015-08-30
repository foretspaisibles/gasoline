(* Gasoline_Generic_Message -- A generic message facility

Author: Michael Grünewald
Date: Thu Aug 23 07:15:48 CEST 2012

Copyright © 2012 Michael Grünewald

This file must be used under the terms of the CeCILL-B.
This source file is licensed as described in the file COPYING, which
you should have received as part of this distribution. The terms
are also available at
http://www.cecill.info/licences/Licence_CeCILL-B_V1-en.txt *)
(* TODO
- Maybe rename DATA to OUTPUT_CHANNEL.
- Maybe narrow down DATA to types word and block.
- Validate identifiers and formats in Lexer.
- Unify interfaces Sender.S and Sink.S. *)
open Printf
open Gasoline_Generic_Definition

type error =
| Invalid_message of string
| Message_not_found of string
| Variable_not_found of string

exception Error of error

let error err =
  raise(Error err)


module Lexer =
struct

  let buffer_sz = 128

  type lexeme =
  | Text of string
  | Variable of string
  | Formatted_variable of string * string

  type pos = {
    mutable char : char;
    mutable char_no : int;
    mutable line_no : int;
    mutable offset: int;
  }

  let reset_pos p =
    p.char <- '\000';
    p.char_no <- 0;
    p.line_no <- 0;
    p.offset <- 0

  let create_pos () = {
    char = '\000';
    char_no = 0;
    line_no = 0;
    offset = 0;
  }

  let update_pos p c =
    p.char <- c;
    p.offset <- p.offset + 1;
    if c = '\n' then begin
      p.char_no <- 0;
      p.line_no <- p.line_no + 1;
    end else begin
      p.char_no <- p.char_no + 1;
    end

  type state =
  | ATX (* Accumulating text *)
  | ESC (* Escape character *)
  | SUB (* Seen substitution sign *)
  | AID (* Accumulating identifier *)
  | AFM of string (* Accumulating format *)

  type t = {
    p : pos;
    b : Buffer.t;
    s : lexeme Stack.t;
  }

  let create () = {
    p = create_pos ();
    b = Buffer.create buffer_sz;
    s = Stack.create();
  }

  let reset lexer =
    begin
      reset_pos lexer.p;
      Buffer.reset lexer.b;
      Stack.clear lexer.s;
    end

  let add_char lexer c =
    Buffer.add_char lexer.b c

  let peek lexer s =
    let p = Stream.peek s in
    begin
      match p with
      | Some c -> update_pos lexer.p c
      | None -> ()
    end;
    p

  type error =
  | Invalid_char_in_identifier of char
  | Invalid_char_in_format of char
  | Unterminated_substitution

  exception Error of pos * error

  let error lexer err =
    raise(Error(lexer.p, err))

  let buffer_finalise b =
    let a = Buffer.contents b in
    Buffer.clear b;
    a

  let is_generic_char set c =
    ('A' <= c && c <= 'Z')
    || ('a' <= c && c <= 'z')
    || ('0' <= c && c <= '9')
    || String.contains set c

  let is_identifier_char c =
    is_generic_char "_" c

  let is_format_char c =
    is_generic_char "_+-.," c

  let rec loop lexer state s =
    let anyway s =
      Stream.next s
    in
    let validate predicate err s =
      let c = Stream.next s in
      if predicate c then
        c
      else
        error lexer (err c)
    in
    let err_invalid_char_in_identifier c =
      Invalid_char_in_identifier c
    in
    let err_invalid_char_in_format c =
      Invalid_char_in_format c
    in
    let validate_identifier s =
      validate is_identifier_char err_invalid_char_in_identifier s
    in
    let validate_format s =
      validate is_format_char err_invalid_char_in_format s
    in
    match state, peek lexer s with
    | ATX  , None -> finalise lexer
    |   _  , None -> error lexer Unterminated_substitution
    | ATX  , Some '\\' -> junk lexer ESC s
    | ATX  , Some '$' -> junk lexer SUB s
    | ATX  , Some  _  -> push anyway lexer ATX s
    | ESC  , Some '$' -> push anyway lexer ATX s
    | ESC  , Some  _  -> resc lexer ATX s
    | SUB  , Some '{'
    | SUB  , Some '(' -> esub lexer AID s
    | SUB  , Some  _  -> rsub lexer ATX s
    | AID  , Some '}'
    | AID  , Some ')' -> pkid lexer ATX s
    | AID  , Some ':' -> junk lexer (AFM(buffer_finalise lexer.b)) s
    | AID  , Some  _  -> push validate_identifier lexer AID s
    | AFM x, Some '}'
    | AFM x, Some ')' -> pkfm lexer ATX s x
    | AFM x, Some  _  -> push validate_format lexer state s
  and junk lexer state s = (* Junk first element of stream and loop *)
    Stream.junk s;
    loop lexer state s
  and push validate lexer state s = (* Push first element of stream and loop *)
    add_char lexer (validate s);
    loop lexer state s
  and resc lexer state s = (* Recover from false escape sequence *)
    add_char lexer '\\';
    loop lexer state s
  and rsub lexer state s = (* Recover from false substitution *)
    add_char lexer '$';
    loop lexer state s
  and esub lexer state s = (* Enter substitution *)
    Stream.junk s;
    finalise_text lexer;
    loop lexer state s
  and pkid lexer state s = (* Pack identifier *)
    Stream.junk s;
    finalise_identifier lexer;
    loop lexer state s
  and pkfm lexer state s id = (* Pack identifier and format *)
    Stream.junk s;
    finalise_identifier_and_format lexer id;
    loop lexer state s
  and finalise_text lexer =
    if Buffer.length lexer.b > 0 then
      Stack.push (Text(buffer_finalise lexer.b)) lexer.s
  and finalise_identifier lexer =
    Stack.push (Variable(buffer_finalise lexer.b)) lexer.s
  and finalise_identifier_and_format lexer id =
    Stack.push (Formatted_variable(buffer_finalise lexer.b, id)) lexer.s
  and finalise lexer =
    finalise_text lexer;
    finalise_loop [] lexer
  and finalise_loop a lexer =
    if Stack.is_empty lexer.s then
      (reset lexer; a)
    else finalise_loop ((Stack.pop lexer.s)::a) lexer

  let analyse lexer s =
    reset lexer;
    loop lexer ATX s

  let analyse_string lexer s =
    analyse lexer (Stream.of_string s)
end


module type P =
sig

  module Data : DATA

  module Locale : LOCALE

  module Buffer : BUFFER
    with type word = Data.word
     and type block = Data.block
     and type out_channel = Data.out_channel

  module Value : VALUE
    with type buffer = Buffer.t
     and type locale = Locale.t

  module MessageDB : MESSAGEDB

  module Priority : PRIORITY
    with type locale = Value.locale
     and type buffer = Buffer.t
     and type out_channel = Data.out_channel
end


module Scribe =
struct

  let accumulator_sz = 1024
  let scratch_sz = 128

  module type S =
  sig
    type t
    type priority
    type connection_token
    type out_channel
    type value
    type locale
    type buffer
    type block
    val make : connection_token -> locale -> t
    val add : t -> priority -> string -> (string * value) list -> unit
    val clear : t -> unit
    val reset : t -> unit
    val contents : t -> block
    val output : out_channel -> t -> unit
    val transfer : buffer -> t -> unit
  end

  module Make(P:P) =
  struct

    type t = {
      database: P.MessageDB.t;
      locale: P.Locale.t;
      accumulator: P.Buffer.t;
      scratch: P.Buffer.t;
      lexer: Lexer.t;
    }

    let make c l = {
      database = P.MessageDB.opendb c;
      locale = l;
      accumulator = P.Buffer.create accumulator_sz;
      scratch = P.Buffer.create scratch_sz;
      lexer = Lexer.create();
    }

    type priority = P.Priority.t
    type connection_token = P.MessageDB.connection_token
    type out_channel = P.Data.out_channel
    type buffer = P.Buffer.t
    type locale = P.Locale.t
    type block = P.Data.block
    type value = P.Value.t

    let clear s =
      P.Buffer.clear s.accumulator

    let reset s =
      begin
        P.Buffer.reset s.accumulator;
        P.Buffer.reset s.scratch;
      end

    let contents s =
      P.Buffer.contents s.accumulator

    let output c s =
      P.Buffer.output_buffer c s.accumulator

    let transfer b s =
      begin
        P.Buffer.add_buffer b s.accumulator;
        clear s
      end

    let add_lexeme scratch locale ax lookup lexeme =
      let open Lexer in
      let open P.Value in
      P.Buffer.clear scratch;
      match lexeme with
      | Text s -> add_text scratch locale ax s
      | Variable v -> add scratch locale ax (lookup v)
      | Formatted_variable(f, v) ->
        add_formatted scratch locale ax (format_of_string f) (lookup v)

    let add s cls id binding =
      let lookup id =
        try List.assoc id binding
        with Not_found -> error (Variable_not_found id)
      in
      let concrete =
        try P.MessageDB.find s.database id
        with Not_found -> error (Message_not_found id)
      in
      let lexemes =
        try Lexer.analyse_string s.lexer concrete
        with Lexer.Error _ -> error (Invalid_message concrete)
      in
      begin
        P.Priority.add_pre s.locale s.accumulator cls id;
        List.iter (add_lexeme s.scratch s.locale s.accumulator lookup) lexemes;
        P.Priority.add_post s.locale s.accumulator cls id;
      end
  end
end


module Sender =
struct

  module type S =
  sig
    type t
    type control = {
      mutable async : bool;
      mutable delay : bool;
      mutable drop : bool;
    }
    val get_control : t -> control

    type priority
    type connection_token
    type out_channel
    type value
    type locale
    val make : connection_token -> locale -> out_channel -> t
    val send : t -> priority -> string -> (string * value) list -> unit
    val sync : t -> unit
    val flush : t -> unit
    val proceed : t -> unit
    val discard : t -> unit
  end

  module Make(P:P) =
  struct

    module Scribe_make = Scribe.Make
    module Scribe = Scribe_make(P)

    type priority =
      P.Priority.t

    type connection_token =
      P.MessageDB.connection_token

    type out_channel =
      P.Data.out_channel

    type value =
      P.Value.t

    type locale =
      P.Value.locale

    type control = {
      mutable async : bool;
      mutable delay : bool;
      mutable drop : bool;
    }

    type message = {
      m_priority : P.Priority.t;
      m_id : string;
      m_binding : (string * value) list;
    }

    let control_create () = {
      async = false;
      delay = false;
      drop = false;
    }

    type t = {
      control : control;
      queue : message Queue.t;
      out_channel : out_channel;
      scribe : Scribe.t;
    }

    let get_control h =
      h.control

    let make token l c = {
      control = control_create();
      queue = Queue.create();
      out_channel = c;
      scribe = Scribe.make token l;
    }

    let really_send h m =
      let cls = m.m_priority in
      let id = m.m_id in
      let binding = m.m_binding in
      let locale = h.scribe.Scribe.locale in
      begin
        Scribe.add h.scribe cls id binding;
        P.Priority.send_pre locale h.out_channel cls id;
        Scribe.output h.out_channel h.scribe;
        P.Priority.send_post locale h.out_channel cls id;
        Scribe.clear h.scribe;
      end

    let really_proceed h =
      if Queue.length h.queue > 0 then
        begin
          Queue.iter (really_send h) h.queue;
          Queue.clear h.queue;
        end

    let send h c id binding =
      let message = {
        m_priority = c;
        m_id = id;
        m_binding = binding;
      } in
      if h.control.drop then
        ()
      else if h.control.delay then
        Queue.add message h.queue
      else begin
        really_proceed h;
        really_send h message;
        if not h.control.async
        then P.Data.flush h.out_channel
      end

    let sync h =
      begin
        really_proceed h;
        P.Data.flush h.out_channel;
        Scribe.reset h.scribe;
      end

    let flush =
      sync

    let proceed =
      sync

    let discard h =
      Queue.clear h.queue
  end
end


(* This private module implements output channel multiplexers. *)
module Out_channel_multiplexer =
struct

  module type S =
  sig
    type t
    val create : unit -> t
    type actual_out_channel
    val add : t -> actual_out_channel -> unit
    val iter : (actual_out_channel -> unit) -> t -> unit
    include DATA with type out_channel = t
  end

  module Make(P:DATA) =
  struct
    type t = P.out_channel Queue.t
    type out_channel = t
    type word = P.word
    type block = P.block
    type actual_out_channel = P.out_channel

    let create =
      Queue.create

    let add m out_c =
      Queue.add out_c m

    let iter =
      Queue.iter

    let output_word m w =
      let output_word_loop w out_c =
        P.output_word out_c w
      in
      iter (output_word_loop w) m

    let output_block m s off len =
      let output_block_loop s off len out_c =
        P.output_block out_c s off len
      in
      iter (output_block_loop s off len) m

    let flush =
      iter P.flush
  end
end


module Sink =
struct

  module type S =
  sig
    type t
    val create : unit -> t

    type priority
    type connection_token
    type out_channel
    type value
    type locale

    type control = {
      mutable async : bool;
      mutable delay : bool;
      mutable drop : bool;
      mutable cap: priority option;
      mutable floor: priority option;
    }
    val get_control : t -> control

    val connect : t -> connection_token -> locale -> unit
    val disconnect : t -> unit
    val is_connected : t -> bool
    val attach : t -> out_channel -> unit
    val detach : t -> unit
    val iter : (out_channel -> unit) -> t -> unit
    val out_channels : t -> out_channel list
    val send : t -> priority -> string -> (string * value) list -> unit
    val sync : t -> unit
    val flush : t -> unit
    val proceed : t -> unit
    val discard : t -> unit
  end

  module Make(P:P) =
  struct

    module Scribe = Scribe.Make(P)
    module Multiplexer = Out_channel_multiplexer.Make(P.Data)

    type priority =
      P.Priority.t

    type connection_token =
      P.MessageDB.connection_token

    type out_channel =
      P.Data.out_channel

    type value =
      P.Value.t

    type locale =
      P.Value.locale

    type control = {
      mutable async : bool;
      mutable delay : bool;
      mutable drop : bool;
      mutable cap: priority option;
      mutable floor: priority option;
    }

    type message = {
      m_priority : P.Priority.t;
      m_id : string;
      m_binding : (string * value) list;
    }

    let control_create () = {
      async = false;
      delay = false;
      drop = false;
      cap = None;
      floor = None;
    }

    module Connection =
    struct
      type t = {
        locale: P.Locale.t;
        database: P.MessageDB.t;
        scribe: Scribe.t;
      }

      let apply_return f x =
        (f x; x)

      let finalise x =
        let finally x =
          P.MessageDB.close x.database
        in
        Gc.finalise finally x

      let make c l =
        let d = P.MessageDB.opendb c in
        apply_return finalise {
        locale = l;
        database = d;
        scribe = Scribe.make c l;
      }

    end

    type t = {
      control : control;
      queue : message Queue.t;
      out_channel : P.Data.out_channel Queue.t;
      mutable connection: Connection.t option;
    }

    let is_connected h = match h.connection with
      | Some _ -> true
      | None -> false

    let only_for_connected f h =
      match h.connection with
      | Some c -> f h c
      | None -> ()

    let get_control h =
      h.control

    let create () = {
      control = control_create();
      queue = Queue.create();
      connection = None;
      out_channel = Queue.create();
    }

    let backend_send h c m =
      let open Connection in
      let cls = m.m_priority in
      let id = m.m_id in
      let binding = m.m_binding in
      let locale = c.locale in
      let backend_send_loop out_channel =
        begin
          P.Priority.send_pre locale out_channel cls id;
          Scribe.output out_channel c.scribe;
          P.Priority.send_post locale out_channel cls id;
        end
      in
      begin
        Scribe.add c.scribe cls id binding;
        Queue.iter backend_send_loop h.out_channel;
        Scribe.clear c.scribe;
      end

    let backend_proceed h c =
      if Queue.length h.queue > 0 then
        begin
          Queue.iter (backend_send h c) h.queue;
          Queue.clear h.queue;
        end

    let backend_flush h =
      Queue.iter P.Data.flush h.out_channel

    let backend_disconnect h c =
      let open Connection in
      let d = c.database in
      begin
        h.connection <- None;
        P.MessageDB.close d;
      end

    let send h cls id binding =
      let really_send h c m =
        begin
          backend_proceed h c;
          backend_send h c m;
          if not h.control.async
          then backend_flush h
        end
      in
      let is_filtered_out h cls =
        let is_greater cls1 cls2 =
          P.Priority.compare cls1 cls2 >= 0
        in
        let is_smaller cls1 cls2 =
          P.Priority.compare cls2 cls2 <= 0
        in
        match h.control.floor, h.control.cap with
        | None, None -> false
        | Some floor, None -> is_greater floor cls
        | None, Some cap -> is_smaller cap cls
        | Some floor, Some cap -> is_greater floor cls || is_smaller cap cls
      in
      let message = {
        m_priority = cls;
        m_id = id;
        m_binding = binding;
      } in
      if h.control.drop || is_filtered_out h cls then
        ()
      else match h.control.delay, h.connection with
      | true, _
      | _, None -> Queue.add message h.queue
      | false, Some c -> really_send h c message

    let sync h =
      let really_sync h c =
        let open Connection in
        begin
          backend_proceed h c;
          backend_flush h;
          Scribe.reset c.scribe;
        end
      in
      only_for_connected really_sync h

    let flush =
      sync

    let proceed =
      sync

    let discard h =
      Queue.clear h.queue

    let disconnect h =
      begin
        sync h;
        only_for_connected backend_disconnect h;
      end

    let connect h l c =
      begin
        disconnect h;
        h.connection <- Some(Connection.make l c);
        proceed h;
      end

    let attach h out_c =
      Queue.add out_c h.out_channel

    let detach h =
      Queue.clear h.out_channel

    let iter f h =
      Queue.iter f h.out_channel

    let out_channels h =
      let loop l x = x :: l in
      Queue.fold loop [] h.out_channel
  end
end
