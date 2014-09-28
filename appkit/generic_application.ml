(* Generic_application -- Generic application

Author: Michael Grünewald
Date: Sun May 12 13:22:40 CEST 2013

Copyright © 2013 Michael Grünewald

This file must be used under the terms of the CeCILL-B.
This source file is licensed as described in the file COPYING, which
you should have received as part of this distribution. The terms
are also available at
http://www.cecill.info/licences/Licence_CeCILL-B_V1-en.txt *)
open SysExits
open Printf

type info =
  Generic_component.info

type callback =
  Generic_component.callback

type component =
  Generic_component.t

module type SINK =
  Generic_message.Sink.S

module type SINK_INITIALIZER =
sig
  type sink
  type connection_token
  type locale
  type out_channel
  val connection_token : info -> connection_token
  val locale : info -> locale
  val out_channel_lst : info -> out_channel list
end

module type P =
sig
  module Value :
  sig
    type 'a kind
    type t
    val make : 'a kind -> 'a -> t
    val to_string : t -> string
    val of_string : string -> t
    val of_string_kind : 'a kind -> string -> 'a
    val kind_name : 'a kind -> string
  end
end

module type S =
sig
  module Component :
  sig
    type info = {
      name : string;
      version : string;
      description : string;
      require: string list;
      provide : string list;
      config_prefix : string list;
      getopt_prefix : char option;
    }
    type sink
    type callback = info -> unit
    val register : ?bootstrap:callback -> ?shutdown:callback -> info -> unit
    val sink : info -> sink
  end
  module Configuration :
  sig
    type 'a kind
    type 'a t
    type component =
      Component.info
    val make : 'a kind -> component ->
      ?flag:char -> ?env:string -> ?shy:bool ->
      string -> 'a -> string -> 'a t
    val get : 'a t -> 'a
    val set : 'a t -> 'a -> unit
    type spec =
    | Empty
    | Command_line
    | Environment
    | File of string
    | RandomFile of string t
    | Heredoc of string
    | Alist of ((string list * string) * string) list
    | Merge of spec * spec
    | Override of spec * spec
  end
  val run : string -> string -> string ->
    ?options:(Getopt.t list) -> ?notes:(Getopt.note list) ->
    ?configuration:Configuration.spec ->
    (string list -> unit) -> unit
  val help : unit -> unit
  val usage : unit -> unit
end

module Make(Sink:SINK)
  (SinkInitializer:SINK_INITIALIZER
   with type sink = Sink.t
   and type connection_token = Sink.connection_token
   and type locale = Sink.locale
   and type out_channel = Sink.out_channel)(Parameter:P) =
struct

  let progname () =
    Filename.basename Sys.executable_name

  let die code argv =
    Printf.fprintf stderr "%s: " (progname());
    Printf.kfprintf
      (fun outc -> output_char outc '\n'; SysExits.exit code) stderr
      argv

  let wlog argv =
    Printf.fprintf stderr "%s: " (progname());
    Printf.kfprintf
      (fun outc -> output_char outc '\n') stderr
      argv

  module Component =
  struct
    include Generic_component

    type sink = Sink.t

    type card = {
      info: info;
      bootstrap: callback;
      shutdown: callback;
    }

    let _expected_sz = 16
    let _card_table = Hashtbl.create _expected_sz
    let _sink_table = Hashtbl.create _expected_sz

    let register ?(bootstrap = ignore) ?(shutdown = ignore) info =
      let card = {
	info;
	bootstrap;
	shutdown;
      } in
      Hashtbl.add _card_table info.name card

    let sink info =
      let create_pack name =
	let sink = Sink.create () in
	Hashtbl.add _sink_table name sink;
	sink
      in
      try Hashtbl.find _sink_table info.name
      with Not_found -> create_pack info.name

    let init () =
      let loop name card =
	Sink.connect (sink card.info)
	  (SinkInitializer.connection_token card.info)
	  (SinkInitializer.locale card.info);
	List.iter (fun outc -> Sink.attach (sink card.info) outc)
	  (SinkInitializer.out_channel_lst card.info)
      in
      Hashtbl.iter loop _card_table

    (* Rules to sort the component list.

    1. If two independent components provide the same thing, then both
       component must be executed to meet the barrier.

    2. If two interdependent components provide the same thing, then
       both component must be executed in graph order to meet the
       barrier.

    3. We pick a component, stack it as “in progress” and try to
       satisfy all its requirements. Once all of an in-progress script
       are met, we can remove it from the “in progress” stack. *)

    let maybe_add x lst =
      if List.mem x lst then lst else x :: lst

    let remove x lst =
      List.filter (fun y -> x <> y) lst

    let dfs graph visited startnode =
      let rec explore path visited node =
        if List.mem node path then
          ksprintf failwith
            "Cyclic dependency on provision '%s'."
            node
        else if List.mem node visited then
          visited
        else
          let nextpath = node :: path in
          let edges = List.assoc node graph in
          let dfsvisited =
            List.fold_left (explore nextpath) visited edges
          in
          node :: dfsvisited
      in
      explore [] visited startnode

    let toposort graph =
      List.fold_left (fun visited (node,_) -> dfs graph visited node)
        [] graph

    let graph table =
      let require card =
	List.map (fun r -> (card.info.name, r)) card.info.require
      in
      let provide card =
	List.map (fun p -> (p, card.info.name)) card.info.provide
      in
      let rec transfer a b =
	match a with
	| [] -> b
	| hd :: tl -> transfer tl (hd::b)
      in
      let edges table =
	let loop _ card ax =
	  ax
	  |> transfer (require card)
	  |> transfer (provide card)
	  |> remove (card.info.name, card.info.name)
	in
	Hashtbl.fold loop table []
      in
      let nodes table =
	let loop _ card ax =
	  (card.info.name, []) :: ax
	in
	Hashtbl.fold loop table []
      in
      let rec add_edge graph ((node, require) as edge) =
	match graph with
	| [] -> [node, [require]]
	| ((hd_node, hd_require) as hd_edge) :: tl ->
	   if hd_node = node then
	     (hd_node, maybe_add require hd_require) :: tl
	   else
	     hd_edge :: add_edge tl edge
      in
      List.fold_left add_edge (nodes table) (edges table)

    let process_components f lst =
      let loop name =
	try f (Hashtbl.find _card_table name)
	with Not_found -> ()
      in
      List.iter loop lst

    let component_graph () =
      graph _card_table

    let bootstrap () =
      component_graph ()
      |> toposort
      |> List.rev
      |> process_components (fun card -> card.bootstrap card.info)

    let shutdown () =
      component_graph ()
      |> toposort
      |> process_components (fun card -> card.shutdown card.info)
  end

  (* Keep trace of environment variables mapped to configurations. *)
  module RegistryEnvironment :
  sig
    (* [add path name env] *)
    val add : string list -> string -> string option -> unit
    val map : unit -> ConfigurationMap.t
  end =
  struct
    let _table = ref []
    let add path name env =
      match env with
      | Some(envname) -> _table := (path, name, envname) :: !_table
      | None -> ()
    let map () =
      let loop ax (path, name, envname) =
	try ConfigurationMap.add ax (path, name) (Sys.getenv envname)
	with Not_found -> ax
      in
      List.fold_left loop ConfigurationMap.empty !_table
  end

  (* Keep trace of command line options mapped to configurations. *)
  module RegistryGetopt :
  sig
    (* [add path name kind flag callback description] *)
    val add : string list -> string ->
      'a Parameter.Value.kind -> char option -> ('a -> unit) -> string -> unit
    val map : unit -> ConfigurationMap.t
    val get : unit -> Getopt.t list
  end = struct
    let _table = ref []
    let really_add path name kind flag callback description =
      let text = ref "" in
      let of_string s =
	text := s;
	Parameter.Value.of_string_kind kind s
      in
      let seen = ref false in
      let callback_wrapper x =
	seen:= true;
	callback x
      in
      let getopt =
	Getopt.concrete of_string flag callback_wrapper description
      in
      _table := (path, name, getopt, text, seen) :: !_table

    let add path name kind flag callback description =
      match flag with
      | Some(c) -> really_add path name kind c callback description
      | None -> ()

    let map () =
      let loop ax (path, name, _, text, seen) =
	try if !seen then
	      ConfigurationMap.add ax (path, name) (!text)
	    else
	      ax
	with Not_found -> ax
      in
      List.fold_left loop ConfigurationMap.empty !_table

    let get () =
      let loop ax (_, _, getopt, _, _) = getopt :: ax in
      List.fold_left loop [] !_table
  end

  module RegistryCallback :
  sig
    val add : ConfigurationMap.callback -> unit
    val iter : (ConfigurationMap.callback -> unit) -> unit
  end = struct
    let _table = ref []
    let add c = _table := c :: !_table
    let iter f= List.iter f !_table
  end

  module Configuration =
  struct
    type 'a kind =
      'a Parameter.Value.kind

    type component =
      Component.info

    type 'a t = 'a ref

    let make kind comp ?flag ?env ?shy name default description =
      let component_path info =
	let open Component in
	info.config_prefix @ [ info.name ]
      in
      let concrete =
	let of_string text =
	  try Parameter.Value.of_string_kind kind text
	  with Failure(_) -> failwith(Parameter.Value.kind_name kind)
	in
	let to_string x =
	  Parameter.Value.make kind x
	  |> Parameter.Value.to_string
	in {
	ConfigurationMap.
	of_string;
	to_string;
	}
      in
      let key = {
	ConfigurationMap.
	concrete;
	path = component_path comp;
	name;
	default;
	description;
      } in
      let item = ref default in
      RegistryGetopt.add (component_path comp) name
	kind flag ((:=) item) description;
      RegistryEnvironment.add (component_path comp) name env;
      RegistryCallback.add
	(ConfigurationMap.callback key ((:=) item));
      item

    let map () =
      ConfigurationMap.empty

    let get item =
      !item

    let set item value =
      item := value

    type spec =
    | Empty
    | Command_line
    | Environment
    | File of string
    | RandomFile of string t
    | Heredoc of string
    | Alist of ((string list * string) * string) list
    | Merge of spec * spec
    | Override of spec * spec

    let handle_failure f x =
      try f x
      with Failure(mesg) -> die EXIT_USAGE "failure: %s" mesg

    let rec map spec =
      let use_file name =
	(* Configuration values used in RandomFile should be
	   initialised to the empty string. *)
	if name <> "" then
	  handle_failure ConfigurationMap.from_file name
	else
	  ConfigurationMap.empty
      in
      match spec with
      | Empty -> ConfigurationMap.empty
      | Command_line -> RegistryGetopt.map ()
      | Environment -> RegistryEnvironment.map ()
      | File(name) -> use_file name
      | RandomFile(name) -> use_file (get name)
      | Heredoc(doc) -> handle_failure ConfigurationMap.from_string doc
      | Alist(bindings) -> ConfigurationMap.from_alist bindings
      | Merge(a,b) -> ConfigurationMap.merge (map a) (map b)
      | Override(a,b) -> ConfigurationMap.override (map a)(map b)

    let init spec =
      let configuration_map = map spec in
      handle_failure RegistryCallback.iter
	    (ConfigurationMap.apply configuration_map)
  end


  module Supervisor =
  struct
    type error =
    | Bootstrap
    | Shutdown
    | Main

    exception Error of error * string

    let supervise err f x =
      let error err mesg =
	raise(Error(err, mesg))
      in
      try f x
      with
      | Failure(mesg) -> error err ("failure: "^mesg)
      | exn -> error err (Printexc.to_string exn)

    let invalid_help () =
      invalid_arg "Generic_application.Make.help"

    let invalid_usage _ =
      invalid_arg "Generic_application.Make.usage"

    let actually_help =
      ref invalid_help

    let actually_usage =
      ref invalid_usage

    let help () =
      !actually_help ()

    let usage mesg =
      !actually_usage mesg

    let terminate mesg mesg2 =
      wlog "%s" mesg;
      die EXIT_SOFTWARE "%s" mesg2

    let emergency_shutdown mesg =
      try
	Component.shutdown();
	die EXIT_SOFTWARE "%s" mesg
      with
      | Failure(mesg2) -> terminate mesg ("failure: "^ mesg2)
      | exn -> terminate mesg (Printexc.to_string exn)

    let run spec main lst =
      try
	supervise Bootstrap Component.bootstrap();
	supervise Main main lst;
	supervise Shutdown Component.shutdown();
      with
      | Error(Bootstrap, mesg) -> die EXIT_SOFTWARE "%s" mesg
      | Error(Shutdown, mesg) -> die EXIT_SOFTWARE "%s" mesg
      | Error(Main, mesg) ->  emergency_shutdown mesg
  end


  let run name usage description
	  ?(options = []) ?(notes = [])
	  ?(configuration = Configuration.Empty)
	  main =
    let restlist = ref [] in
    let rest s = restlist := s :: !restlist in
    let spec () =
      Getopt.spec usage description
	(RegistryGetopt.get () @ options) rest notes
    in
    begin
      Component.init ();
      Getopt.parse_argv (spec());
      Configuration.init configuration;
      Supervisor.run (spec()) main !restlist;
    end

  let help () =
    Supervisor.help ()

  let usage mesg =
    Supervisor.usage mesg
end
