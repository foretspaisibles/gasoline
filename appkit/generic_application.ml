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
  end
  val run : string -> string -> string ->
    ?options:(Getopt.t list) -> ?notes:(Getopt.note list) ->
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

  let die code argv =
    Printf.kfprintf
      (fun outc -> output_char outc '\n'; SysExits.exit code) stderr
      argv

  let wlog argv =
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
      wlog "register: %s" info.name;
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
	wlog "init: %s" name;
	Sink.connect (sink card.info)
	  (SinkInitializer.connection_token card.info)
	  (SinkInitializer.locale card.info);
	List.iter (fun outc -> Sink.attach (sink card.info) outc)
	  (SinkInitializer.out_channel_lst card.info)
      in
      Hashtbl.iter loop _card_table

    let rec recursive_require name =
      let card = Hashtbl.find _card_table name in
      List.concat
	(card.info.require :: List.map recursive_require card.info.require)

    let provide name =
      (Hashtbl.find _card_table name).info.provide

    let is_required_by a b =
      let bdependents = recursive_require b in
      let required_by_b name =
	List.mem name bdependents
      in
      List.exists required_by_b (a :: provide a)

    let must_bootstrap_before a b =
      let general_case a b =
	match is_required_by a b, is_required_by b a with
	| true, true -> failwith "cyclic dependency"
	| true, false -> true
	| _ -> false
      in
      a <> b && general_case a b

    let must_shutdown_before a b =
      must_bootstrap_before b a

    let rcorder is_before lst =
      let compare a b =
	if is_before a b then
	  1
	else if a = b then
	  0
	else
	  -1
      in
      List.sort compare lst

    let rcorder_apply is_before f =
      let loop name =
	f (Hashtbl.find _card_table name)
      in
      let component_lst =
	Hashtbl.fold (fun name _ ax -> name :: ax) _card_table []
      in
      List.iter loop (rcorder is_before component_lst)

    let bootstrap () =
      rcorder_apply must_bootstrap_before (fun card -> card.bootstrap card.info)

    let shutdown () =
      rcorder_apply must_shutdown_before (fun card -> card.shutdown card.info)
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
      let concrete = {
	ConfigurationMap.
	of_string = Parameter.Value.of_string_kind kind;
	to_string = (fun x -> Parameter.Value.make kind x
			|> Parameter.Value.to_string );
      } in
      let key = {
	ConfigurationMap.
	concrete;
	path = component_path comp;
	name;
	default;
	description;
      } in
      let item = ref default in
      let callback = ConfigurationMap.callback key ((:=) item) in
      RegistryEnvironment.add (component_path comp) name env;
      (* TODO: Create command line option creating a map entry *)
      (* TODO: Store the callback somewhere *)
      item

    let map () =
      wlog "Using empty configuration map";
      ConfigurationMap.empty

    let get item =
      !item

    let set item value =
      item := value
  end


  let run name usage description ?options ?notes main =
(*    init_sink_system ();
    init_components ();
    prepare_configuration_maps();
    build_command_line_spec();
    execute_command_line_spec();
    main rest
*)
    begin
      Component.init ();
      Component.bootstrap();
      main [];
      Component.shutdown();
    end

  let help () =
    failwith "CApplication.help: not implemented"

  let usage () =
    failwith "CApplication.usage: not implemented"
end
