(* Generic_application -- Generic application

Author: Michael Grünewald
Date: Sun May 12 13:22:40 CEST 2013

Copyright © 2013 Michael Grünewald

This file must be used under the terms of the CeCILL-B.
This source file is licensed as described in the file COPYING, which
you should have received as part of this distribution. The terms
are also available at
http://www.cecill.info/licences/Licence_CeCILL-B_V1-en.txt *)
(* TODO(michipili) Ironing error conditions
- Software component have cyclic dependencies
- Cannot parse a value on the command line
- Cannot parse a value in a configuration file
- Cannot parse a value in an environment variable
- Cannot parse a configuration file
- Cannot initialise a sink
*)

(* TODO (michipili) Introduce suitable default paths for non-Unix *)

open Printf

let component_table_sz = 32
let buffer_sz = 1024

type component = {
  name : string;
  version : string;
  require: string list;
  description: string;
  config_prefix : string list;
  getopt_prefix : char;
}

type callback = unit -> unit

type 'a concrete =
  'a Configuration.concrete

type configuration_spec =
| Empty
| Command_line
| Environment
| File of string
| Heredoc of string
| Alist of ((string list * string) * string) list
| Merge of configuration_spec * configuration_spec
| Override of configuration_spec * configuration_spec

module Character_set =
  Set.Make(Char)

module Identification =
struct

  let _name =
    ref(Filename.basename Sys.argv.(0))

  let _version =
    ref "0.0"

  let _usage =
    ref ""

  let _description =
    ref ""

  let _copyright =
    ref ""

  let _license =
    ref ""

  let _author =
    ref []

  let setup f ~name ~version ~usage ~description ~copyright ~license ~author =
    begin
      _name := name;
      _version := version;
      _usage := usage;
      _description := description;
      _copyright := copyright;
      _license := license;
      _author := author;
      f
    end
end

exception Error of int * string

let error code fmt =
  let f text =
    raise (Error(code, text))
  in
  ksprintf f fmt

let execute f x =
  try f x
  with
  | Error(_, msg)-> eprintf "%s: %s\n" !Identification._name msg


module Path =
struct
  let _prefix = ref ""
  let _bindir = ref ""
  let _sbindir = ref ""
  let _libexecdir = ref ""
  let _datadir = ref ""
  let _sysconfdir = ref ""
  let _usrconfdir = ref ""
  let _sharedstatedir = ref ""
  let _localstatedir = ref ""

  let meta_default list arg =
    match arg with
    | Some p -> p
    | None -> List.fold_left Filename.concat !_prefix list

  let abs_default path arg =
    match arg with
    | Some p -> p
    | None -> path

  let site_default path arg =
    meta_default [ path ] arg

  let prog_default path arg =
    meta_default [ path; !Identification._name ] arg

  let setup f ?(prefix = "/usr/local")
      ?bindir ?sbindir ?libexecdir ?datadir ?sysconfdir ?usrconfdir
      ?sharedstatedir ?localstatedir =
    let apply (r,f,x) =
      r := f x
    in
    begin
      _prefix := prefix;
      List.iter apply [
	_bindir, site_default "bin", bindir;
	_sbindir, site_default "sbin", sbindir;
	_libexecdir, prog_default "libexec", libexecdir;
	_datadir, prog_default "share", datadir;
	_sysconfdir, prog_default "etc", sysconfdir;
	_usrconfdir, abs_default (Sys.getenv "HOME"), usrconfdir;
	_sharedstatedir, prog_default "com", sharedstatedir;
	_localstatedir, prog_default "var", localstatedir;
      ];
      f
    end
end


module type S =
sig
  val identify :
    name:string (** Name of the application *) ->
    version:string (** Version information *) ->
    usage:string (** Usage on the command line *) ->
    description:string (** One-line description of the application. *) ->
    copyright:string (** Copyright notice *) ->
    license:string (** License notice *) ->
    author:string list (** Author *) ->
    ?prefix:string (** Installation prefix *) ->
    ?bindir:string (** User programs directory *) ->
    ?sbindir:string (** System administrator programs directory *) ->
    ?libexecdir:string (** Ancillary programs directory *) ->
    ?datadir:string (** Read-only architecture-independant data files *) ->
    ?sysconfdir:string (** Host configuration, read-only *) ->
    ?usrconfdir:string (** User configuration, read-only *) ->
    ?sharedstatedir:string (** Local state directory, e.g. /com *) ->
    ?localstatedir:string (** Local state directory, e.g. /var *) ->
    unit -> unit
  val bin : string -> string
  val sbin : string -> string
  val libexec : string -> string
  val data : string -> string
  val sysconf : string -> string
  val usrconf : string -> string
  val sharedstate : string -> string
  val localstate : string -> string
  val register : ?bootstrap:callback -> ?shutdown:callback -> component -> unit
  val iter : (component -> unit) -> unit
  val fold : (component -> 'a -> 'a) -> 'a -> 'a
  type sink
  val sink : component -> sink
  val config : 'a concrete -> component -> ?flag:char -> ?env:string -> ?shy:bool -> string -> 'a -> string -> 'a ref
  val flag : Getopt.t -> unit
  val note : string -> string -> unit
  val main : configuration_spec -> (string -> unit) -> (unit -> unit) -> unit
end

module type SINK =
  Generic_message.Sink.S

module type P =
sig
  type sink
  val register : (Configuration.handler -> unit) -> ((unit -> unit) -> unit) -> component -> sink -> unit
end

module Component =
struct
  type t = component
  let equal = (=)
  let hash = Hashtbl.hash
  let compare = Pervasives.compare
  let name comp =
    comp.name
  let require comp =
    comp.require
end

module Component_table =
  Hashtbl.Make(Component)

module Component_set =
  Set.Make(Component)

module Make(Sink:SINK)(Parameter:P with type sink = Sink.t) =
struct
  type sink = Sink.t

  (* Our component registry *)
  module Bureaucracy =
  struct
    let _component =
      ref Component_set.empty

    let register comp =
      _component := Component_set.add comp !_component

    let components () =
      Component_set.elements !_component

    let iter f =
      Component_set.iter f !_component

    let fold loop ax =
      Component_set.fold loop !_component ax
  end


  (* Configuration keys *)
  module Configuration_key =
  struct
    let table =
      Queue.create ()

    let add_new item value =
      Queue.add (Configuration.handler item ((:=) value)) table

    let add key =
      Queue.add key table

    let apply c =
      Queue.iter (Configuration.apply c) table
  end


  (* Command line *)
  module Command_line =
  struct
    let flag_table =
      Queue.create ()

    let add_newflag c cb description =
      Queue.add (Getopt.string c cb description) flag_table

    let add_flag f =
      Queue.add f flag_table

    let long_table =
      Queue.create ()

    let add_newlong c key cb description =
      Queue.add (c, key, cb, description) long_table

    let note_table =
      Queue.create ()

    let add_note n =
      Queue.add n note_table

    let add_newnote title body =
      add_note (Getopt.note title body)

    let long_letter_set () =
      let loop ax (c,_,_,_) = Character_set.add c ax in
      Queue.fold loop Character_set.empty long_table

    let long_letter_make c =
      let buffer = Buffer.create buffer_sz in
      let loop ax (c, key, cb, description) =
	begin
	  Buffer.add_string buffer description;
	  (Getopt.long_string key cb) :: ax
	end
      in
      let l = Queue.fold loop [] long_table in
      Getopt.long c l (Buffer.contents buffer)

    let long_make l =
      let set = long_letter_set () in
      let loop c ax = (long_letter_make c) :: ax in
      Character_set.fold loop set l

    let make usage description rest =
      let table_elements table =
	Queue.fold (fun a b -> b :: a) [] table
      in
      let options = long_make (table_elements flag_table) in
      let notes = table_elements note_table in
      Getopt.spec usage description options rest notes

    let parse_argv rest argv =
      let spec =
	make
	  !Identification._usage
	  !Identification._description
	  rest
      in
      Getopt.parse spec argv
  end


  (* Configuration files *)
  module Configuration_file =
  struct
    let make f =
      try Configuration.from_file f
      with Sys_error msg -> (
	(* TODO(michipili) Canonise error *)
	(* TODO(michipili) Catch parse error *)
	eprintf "%s: configuration: %s\n"
	  !Identification._name msg;
	Configuration.empty
      )
  end


  (* Configuration from environment *)
  module Configuration_environment =
  struct
    type t = {
      path: string list;
      key: string;
      variable: string;
    }

    let table =
      Queue.create ()

    let add item var =
      Queue.add {
	path = item.Configuration.path;
	key = item.Configuration.name;
	variable = var;
      }

    let make () =
      let loop ax r =
	try Configuration.add ax (r.path, r.key) (Sys.getenv r.variable)
	with Not_found -> ax
      in
      Queue.fold loop Configuration.empty table
  end


  (* Configuration from command line arguments *)
  module Configuration_getopt =
  struct
    type t = {
      path: string list;
      key: string;
      mutable value: string option;
    }

    let table =
      Queue.create ()

    let make_callback item =
      let r = {
	path = item.Configuration.path;
	key = item.Configuration.name;
	value = None;
      } in
      let callback s =
	r.value <- Some s
      in
      begin
	Queue.add r table;
	callback
      end

    let add_flag item flag =
      let callback = make_callback item in
      Command_line.add_newflag flag callback item.Configuration.description

    let add_long item flag =
      let callback = make_callback item in
      Command_line.add_newlong
	flag
	item.Configuration.name
	callback
	item.Configuration.description

    let make () =
      let loop ax r =
	match r.value with
	| Some v -> Configuration.add ax (r.path, r.key) v
	| None -> ax
      in
      Queue.fold loop Configuration.empty table
  end

  module Configuration_policy =
  struct
    let rec apply policy =
      match policy with
      | Empty -> Configuration.empty
      | Environment -> Configuration_environment.make ()
      | Command_line -> Configuration_getopt.make ()
      | File s -> Configuration_file.make s
      | Heredoc s -> Configuration.from_string s
      | Alist a -> Configuration.from_alist a
      | Merge(a,b) -> Configuration.merge  (apply a) (apply b)
      | Override(a,b) -> Configuration.override (apply a) (apply b)

    let rec merge ax = match ax with
      | [] -> Empty
      | a :: [] -> a
      | a :: b  -> Merge(a, merge b)
  end


  (* Configuration *)

  module Incidence =
  struct

    let list () =
      let loop comp ax =
	(comp.name, comp.require) :: ax
      in
      Bureaucracy.fold loop []

    (* a bigger than b iff a depends on b *)
    let compare_dep a b =
      let tbl = list () in
      let incidence x =
	try List.assoc x tbl
	with Not_found -> []
      in
      let rec loop areq breq =
	match areq, breq, List.mem a.name breq, List.mem b.name areq with
	| [], [], _, _ -> 0
	| _, _, true, true ->
	  error 70 "component: cyclic dependency: %s, %s" a.name b.name
	| _, _, true, false -> 1
	| _, _, false, true -> -1
	| ha :: ta, _, false, false ->
	  loop (ta @ (incidence ha)) breq
	| _, hb :: tb, false, false ->
	  loop areq (tb @ (incidence hb))
      in
      if a = b
      then
	0
      else
	loop (incidence a.name) (incidence b.name)

    (* a is greater than b iff a is required by b *)
    let compare_rev a b =
      compare_dep b a

    let bootstrap_list () =
      List.sort compare_dep (Bureaucracy.components ())

    let shutdown_list () =
      List.sort compare_rev (Bureaucracy.components ())

    let initialise () =
      begin
	(* Ensure that there is no cyclic dependancy between components. *)
	ignore (bootstrap_list());
	ignore (shutdown_list());
      end
  end

  module Component_management =
  struct
    module type P =
    sig
      val order : unit -> component list
    end
    module Make(Parameter:P) =
    struct
      let _table =
	Component_table.create component_table_sz

      let register comp a =
	match a with
	| Some cb -> Component_table.add _table comp cb
	| None -> ()

      let process () =
	let loop comp =
	  let cb =
	    try Component_table.find _table comp
	    with Not_found -> ignore
	  in
	  cb()
	in
	List.iter loop (Parameter.order())
    end
  end

  module Bootstrap =
    Component_management.Make(
      struct let order = Incidence.bootstrap_list end
    )

  module Shutdown =
    Component_management.Make(
      struct let order = Incidence.shutdown_list end
    )

  module Component_sink =
  struct
    let sink_table =
      Component_table.create component_table_sz

    let callbacks =
      Queue.create ()

    let register_callback cb =
      Queue.add cb callbacks

    let maybe_create comp =
      let pack_create () =
	let s = Sink.create() in
	begin
	  Component_table.add sink_table comp s;
	  s
	end
      in
      try Component_table.find sink_table comp
      with Not_found -> pack_create ()

    let add_configuration_keys () =
      let loop comp sink =
	Parameter.register
	  Configuration_key.add
	  register_callback
	  comp sink
      in
      Component_table.iter loop sink_table

    let initialise () =
      let loop cb =
	cb ()
      in
      Queue.iter loop callbacks
  end


  let identify =
    Identification.setup (Path.setup ignore)

  let progname () =
    !Identification._name

  let bin =
    Filename.concat !Path._bindir

  let sbin =
    Filename.concat !Path._sbindir

  let libexec =
    Filename.concat !Path._libexecdir

  let data =
    Filename.concat !Path._datadir

  let sysconf =
    Filename.concat !Path._sysconfdir

  let usrconf =
    Filename.concat !Path._usrconfdir

  let sharedstate =
    Filename.concat !Path._sharedstatedir

  let localstate =
    Filename.concat !Path._localstatedir

  let rcfile () =
    usrconf (sprintf ".%s" (progname ()))

  let sysconffile () =
    sysconf (sprintf "%s.conf" (progname ()))

  let register ?bootstrap ?shutdown comp =
    begin
      Bureaucracy.register comp;
      Bootstrap.register comp bootstrap;
      Shutdown.register comp shutdown;
    end

  let iter =
    Bureaucracy.iter

  let fold =
    Bureaucracy.fold

  let sink =
    Component_sink.maybe_create

  let config concrete comp ?flag ?env ?(shy = false)
      name default description =
    let config_value =
      ref default
    in
    let config_item = {
      Configuration.
      concrete = concrete;
      path = comp.config_prefix;
      name = name;
      default = default;
      description = description;
    }
    in
    begin
      Configuration_key.add_new config_item config_value;
      (
	match flag with
	| Some c -> Configuration_getopt.add_flag config_item c
	| None -> ()
      );
      Configuration_getopt.add_long config_item comp.getopt_prefix;
      config_value
    end

  let flag =
    Command_line.add_flag

  let note =
    Command_line.add_newnote

  let main policy rest f =
    begin
      Component_sink.add_configuration_keys();
      Command_line.parse_argv rest Sys.argv;
      Configuration_key.apply (Configuration_policy.apply policy);
      Component_sink.initialise();
      Bootstrap.process();
      f ();
      Shutdown.process();
    end
end
