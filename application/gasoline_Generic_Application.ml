(* Gasoline_Generic_Application -- Generic application

Author: Michael Grünewald
Date: Sun May 12 13:22:40 CEST 2013

Copyright © 2013 Michael Grünewald

This file must be used under the terms of the CeCILL-B.
This source file is licensed as described in the file COPYING, which
you should have received as part of this distribution. The terms
are also available at
http://www.cecill.info/licences/Licence_CeCILL-B_V1-en.txt *)

type component = {
  name : string;
  version : string option;
  description : string;
  require: string list;
  provide : string list;
  config_prefix : string list;
  getopt_prefix : char option;
  bootstrap: unit -> unit;
  shutdown: unit -> unit;
}

type error =
  | Bootstrap of string * (string list)
  (* Bootstrap(name, lst) signals an error while bootstrapping
     the component [name]. The list [lst] enumerates the components
     which have been correctly bootstrapped. *)

  | Shutdown of string * (string list)
  (* Shutdown(mesg, name, lst) signals an error while shutting down
     the component [name]. The list [lst] enumerates the components
     which still need to be shut down. *)

  | Software
  | Usage

let ( $ ) f g =
  fun x -> f (g x)

let memoize f =
  let m = ref None in
  fun () -> match !m with
    | None -> (fun x -> m := Some(x); x) (f())
    | Some(x) -> x

module Success =
  Lemonade_Success.Make(struct type t = error end)

module Maybe =
  Lemonade_Maybe

type validate =
  (string -> string -> string -> string Success.t) -> string Success.t Configuration_Map.key

let progname () =
  Filename.basename Sys.executable_name

let wlog mesg =
  Printf.eprintf "%s: %s\n" (progname()) mesg

let die code fmt =
  let quit () =
    Gasoline_SysExits.exit code
  in
  Printf.ksprintf (quit $ wlog) fmt

let _error error fmt =
  Printf.ksprintf (fun mesg -> wlog mesg; Success.throw error) fmt

let error fmt =
  _error Software fmt

let error_bootstrap name lst fmt =
  _error (Bootstrap(name, lst)) fmt

let error_shutdown name lst fmt =
  _error (Shutdown(name, lst)) fmt

let error_usage fmt =
  _error Usage fmt

let error_getopts usage fmt =
  Printf.ksprintf
    (fun mesg ->
       wlog mesg;
       Printf.eprintf "Usage: %s %s\n" (progname()) usage;
       Success.throw Usage)
    fmt

let dfs graph visited startnode =
  let rec explore path visited node =
    if List.mem node path then
      Printf.ksprintf failwith
        "Cyclic dependency on component '%s'."
        node
    else if List.mem node visited then
      visited
    else
      let nextpath = node :: path in
      let edges =
        try List.assoc node graph
        with Not_found -> []
      in
      let dfsvisited =
        List.fold_left (explore nextpath) visited edges
      in
      node :: dfsvisited
  in
  explore [] visited startnode

let toposort graph =
  try
    Success.return
    @@ List.fold_left
      (fun visited (node,_) -> dfs graph visited node)
      [] graph
  with Failure(mesg) -> error "Failure: %s" mesg


module type P =
sig
end

module type S =
sig
  module Component :
  sig
    type t
    val make :
      ?bootstrap:(unit -> unit) ->
      ?shutdown:(unit -> unit) ->
      ?require:(string list) ->
      ?provide:(string list) ->
      ?version:string ->
      ?config_prefix: string list ->
      ?getopt_prefix: char ->
      name:string ->
      description:string ->
      unit -> t
  end

  module Configuration :
  sig
    type component =
      Component.t

    val make : (string -> 'a) -> component ->
      ?optarg:string ->
      ?flag:char -> ?env:string -> ?shy:bool ->
      string -> 'a -> string -> (unit -> 'a)

    type spec =
    | Empty
    | Command_line
    | Environment
    | OptionalFile of string
    | ImportantFile of string
    | UserFile of string list * string
    | Heredoc of string
    | Alist of ((string list * string) * string) list
    | Merge of spec * spec
    | Override of spec * spec
  end

  val run : string -> string -> string ->
    ?notes:((string * string) list) ->
    ?configuration:Configuration.spec ->
    (string list -> unit) -> unit
end

module Make(Parameter:P) =
struct

  module Component =
  struct
    type t = component

    let _expected_sz =
      16

    let _component_table =
      Hashtbl.create _expected_sz

    let make
        ?(bootstrap = ignore)
        ?(shutdown = ignore)
        ?(require = [])
        ?(provide = [])
        ?version
        ?(config_prefix = [])
        ?getopt_prefix
        ~name
        ~description
        ()
      =
      let comp = {
        bootstrap;
        shutdown;
        require;
        provide;
        version;
        config_prefix;
        getopt_prefix;
        name;
        description;
      }
      in
      (Hashtbl.add _component_table name comp; comp)

    module NodeSet =
      Set.Make(String)

    module EdgeSet =
      Set.Make(struct
        type t = string * string
        let compare = Pervasives.compare
      end)

    (* Rules to sort the component list.

       1. If two independent components provide the same thing, then
       both component must be executed to meet the barrier.

       2. If two interdependent components provide the same thing,
       then both component must be executed in graph order to meet the
       barrier.

       3. We pick a component, stack it as “in progress” and try to
       satisfy all its requirements. Once all of an in-progress script
       are met, we can remove it from the “in progress” stack. *)

    let _graphspec table =
      Hashtbl.fold
        (fun _ comp acc -> (comp.name, (comp.provide, comp.require)) :: acc)
        table
        []

    let _graph spec =
      let require (name, (_, requirelst)) =
        List.map (fun r -> (name, r)) requirelst
      in
      let provide (name, (providelst, _)) =
        List.map (fun p -> (p, name)) providelst
      in
      let edges spec =
        let loop ax comp =
          ax
          |> List.fold_right EdgeSet.add (require comp)
          |> List.fold_right EdgeSet.add (provide comp)
        in
        List.fold_left loop EdgeSet.empty spec
        |> EdgeSet.filter (fun edge -> fst edge <> snd edge)
        |> EdgeSet.elements
      in
      let nodes spec =
        List.fold_left (fun acc (node, (providelst, requirelst)) ->
            NodeSet.union acc (NodeSet.of_list (node :: providelst @ requirelst)))
          NodeSet.empty spec
        |> NodeSet.elements
      in
      let select edges node =
        (node, List.map snd (List.filter (fun (x,_) -> x = node) edges))
      in
      List.map (select (edges spec)) (nodes spec)

    let rcorder () =
      let barrier =
        Hashtbl.fold
          (fun _ comp acc ->
             List.rev_append (List.filter ((<>) comp.name) comp.provide) acc)
          _component_table
          []
        |> NodeSet.of_list
      in
      let _filter lst =
        List.filter (fun x -> not(NodeSet.mem x barrier)) lst
      in
      Success.bind
        (toposort (_graph (_graphspec _component_table)))
        (Success.return $ _filter)

    let process f acc lst =
      let open Success.Infix in
      let rec loop m name =
        Success.bind2
          (try Success.return(Hashtbl.find _component_table name)
           with Not_found -> error "%s: No such component." name)
          m f
      in
      List.fold_left loop (Success.return acc) lst

    let bootstrap () =
      let open Success.Infix in
      let f comp acc =
        try (comp.bootstrap (); Success.return(comp.name :: acc))
        with
        | Failure(mesg) -> error_bootstrap comp.name acc "Bootstrap: %s: %s" comp.name mesg
        | exn -> error_bootstrap comp.name acc "Bootstrap: %s: %s" comp.name (Printexc.to_string exn)
      in
      rcorder ()
      >>= (Success.return $ List.rev)
      >>= process f []
      >>= (Success.return $ ignore)

    let shutdown () =
      let open Success.Infix in
      let f comp acc =
        let pending = List.filter ((<>) comp.name) acc in
        try (comp.shutdown (); Success.return pending)
        with
        | Failure(mesg) -> error_shutdown comp.name pending "Shutdown: %s: %s" comp.name mesg
        | exn -> error_shutdown comp.name pending "Shutdown: %s: %s" comp.name (Printexc.to_string exn)
      in
      let rec loop lst =
        Success.catch
          (process f lst lst)
          (function
            | Shutdown(name, pending) as error ->
                (loop pending >>= fun _ -> Success.throw error)
            | whatever -> Success.throw whatever)
      in
      rcorder ()
      >>= loop
      >>= function
      | [] -> Success.return ()
      | whatever ->
          error
            "%s: Skept by the shutdown procedure."
            (String.concat ", " whatever)

    let fold f acc =
      Hashtbl.fold f _component_table acc
  end

  (* Keep trace of environment variables mapped to configurations. *)
  module Configuration_Environment :
  sig
    (* [add path name env] *)
    val add : validate -> string-> unit
    val query : unit -> Configuration_Map.t Success.t
  end = struct
    let _table = ref []

    let add validatekey env =
      _table := (validatekey, env) :: !_table

    let _query () =
      let loop ax (validatekey, envname) =
        let catch kindname text _ =
          error_usage "Invalid Argument -- %s %S: Bad %s value."
            envname text kindname
        in
        let configkey =
          validatekey catch
        in
        try
          Success.map2
            (fun optarg acc -> Configuration_Map.
                             (add (configkey.path, configkey.name) optarg acc))
            (Configuration_Map.value configkey (Sys.getenv envname))
            ax
        with Not_found -> ax
      in
      List.fold_left loop (Success.return Configuration_Map.empty) !_table

    let query =
      memoize _query
  end

  (* Keep trace of command line options mapped to configurations. *)
  module Configuration_Getopts :
  sig
    val init : string -> string -> (string*string) list -> unit
    (* [add path name flag description] *)
    val add : ?optarg:string -> validate -> char -> unit
    val add_long : ?optarg:string -> validate -> char -> string -> unit
    val query : unit -> Configuration_Map.t Success.t
    val rest : unit -> string list Success.t
  end = struct
    let _table = ref []
    let _long = ref []
    let _context = ref None

    let spec_short () =
      !_table

    let spec_long () =
      let flags =
        let module Pool = Set.Make(Char) in
        Pool.elements
          (List.fold_left (fun acc (c, _) -> Pool.add c acc) Pool.empty !_long)
      in
      let options c =
        List.map snd (List.filter (fun (k, _) -> k = c) !_long)
      in
      let description _ =
        (* Make shy options *)
        ""
      in
      List.map (fun c -> Getopts.long c (options c) (description c)) flags

    let spec () =
      let open Success.Infix in
      (match !_context with
       | Some(whatever) -> Success.return whatever
       | None -> error "Configuration_Getopt: Context not initialised.")
      >>= fun (usage, description, notes) ->
      Success.return
        (Getopts.spec usage description (spec_long () @ spec_short ())
           (fun arg m ->
              (Success.map (fun (config, rest) -> (config, arg :: rest)) m))
           (List.map (fun (title, body) -> Getopts.note title body) notes))

    let init usage description notes =
      _context := Some(usage, description, notes)

    let add ?optarg validatekey flag =
      let catch kindname text _ =
        match !_context with
        | None ->
            error_usage "Invalid Argument -- %c %S: Bad %s value."
              flag text kindname
        | Some(usage,_,_) ->
            error_getopts usage "Invalid Argument -- %c %S: Bad %s value."
              flag text kindname
      in
      let configkey =
        validatekey catch
      in
      let fold validoptarg (config, rest) =
        Configuration_Map.(add (configkey.path, configkey.name)
                             validoptarg config, rest)
      in
      let option =
        match optarg with
        | None ->
            Getopts.option
              (fun s -> s)
              flag
              (fun optarg m ->
                 (Success.map2 fold (Configuration_Map.value configkey optarg) m))
              configkey.Configuration_Map.description
        | Some(text) ->
            Getopts.flag
              flag
              (fun m ->
                 (Success.map2 fold (Configuration_Map.value configkey text) m))
              configkey.Configuration_Map.description

      in
      _table := option :: !_table

    let add_long ?optarg validatekey flag name =
      let catch kindname text _ =
        match !_context with
        | None ->
            error_usage "Invalid Argument -- %c %s %S: Bad %s value."
              flag name text kindname
        | Some(usage,_,_) ->
            error_getopts usage "Invalid Argument -- %c %s %S: Bad %s value."
              flag name text kindname
      in
      let configkey =
        validatekey catch
      in
      let fold validoptarg (config, rest) =
        Configuration_Map.(add (configkey.path, configkey.name)
                             validoptarg config, rest)
      in
      let option =
        match optarg with
        | None ->
            Getopts.long_option
              (fun s -> s)
              name
              (fun optarg m ->
                 (Success.map2 fold (Configuration_Map.value configkey optarg) m))
        | Some(text) ->
            Getopts.long_flag
              name
              (fun m ->
                 (Success.map2 fold (Configuration_Map.value configkey text) m))

      in
      _long := (flag, option) :: !_long

    let _parse () =
      let open Success.Infix in
      spec () >>= fun s ->
      (Getopts.parse_argv s (Success.return (Configuration_Map.empty, [])))
      >>= fun (config, rest) -> Success.return(config, List.rev rest)

    let _memo_parse =
      memoize _parse

    let query () =
      let open Success.Infix in
      _memo_parse () >>= (Success.return $ fst)

    let rest () =
      let open Success.Infix in
      _memo_parse () >>= (Success.return $ snd)
  end

  module Configuration_File :
  sig
    (* [add path name env] *)
    val add : validate -> unit
    val parse : bool -> string -> Configuration_Map.t Success.t
    val heredoc : string -> Configuration_Map.t Success.t
  end = struct
    let _table = ref []
    let add validatekey =
      _table := validatekey :: !_table

    let _validate config =
      let challenge m validatekey =
        Success.bind
          m
          (fun () -> Success.map ignore
              (try Configuration_Map.get config (validatekey failwith)
               with Failure(mesg) -> error_usage "Failure: %s" mesg))
      in
      Success.map
        (fun () -> config)
        (List.fold_left challenge (Success.return()) !_table)

    let parse important name =
      (* Configuration values used in UserFile should be
           initialised to the empty string. *)
      let open Success.Infix in
      (if name <> "" then
         try Success.return(Configuration_Map.from_file name)
         with
         | Failure(mesg) -> error "Failure: %s" mesg
         | Sys_error(mesg) ->
             if important then
               error "Error: %s" mesg
             else
               Success.return(Configuration_Map.empty)
         | exn -> error "%s: %s" name (Printexc.to_string exn)
       else
         Success.return(Configuration_Map.empty))
      >>= _validate

    let heredoc data =
      let open Success.Infix in
      (try Success.return(Configuration_Map.from_string data)
       with
         | Failure(mesg) -> error "Failure: %s" mesg
         | exn -> error "%s" (Printexc.to_string exn))
      >>= _validate
  end

  module Configuration =
  struct
    type component =
      Component.t

    let _config =
      ref Configuration_Map.empty

    let _hidden =
      (* The '#' character is used for comments in configuration files,
         so that no identifier will ever start with '#'. *)
      let count = ref 0 in
      fun () -> (incr count; Printf.sprintf "#%12d" !count)

    let query () =
      !_config

    let make value_of_string comp
        ?optarg ?flag ?env ?shy name default description =
      let component_path comp =
        comp.config_prefix @ [ comp.name ]
      in
      let configkey =
        Configuration_Map.key
          value_of_string
          (component_path comp)
          name
          default
          description
      in
      let validate catch text =
        try (ignore(value_of_string text);
             Success.return text)
        with Failure(mesg) -> catch mesg text mesg
      in
      let validatekey catch =
        Configuration_Map.key
          (validate catch)
          (component_path comp)
          name
          (Success.return "")
          description
      in
      let open Configuration_Map in
      (match flag with
       | Some c -> Configuration_Getopts.add ?optarg validatekey c
       | None -> ());
      (match comp.getopt_prefix with
       | Some c -> Configuration_Getopts.add_long ?optarg validatekey c name
       | None -> ());
      (match env with
       | Some id -> Configuration_Environment.add validatekey id
       | None -> ());
      (match env with
       | Some id -> Configuration_File.add validatekey
       | None -> ());
      (fun () -> Configuration_Map.get !_config configkey )

    type spec =
      | Empty
      | Command_line
      | Environment
      | OptionalFile of string
      | ImportantFile of string
      | UserFile of string list * string
      | Heredoc of string
      | Alist of ((string list * string) * string) list
      | Merge of spec * spec
      | Override of spec * spec

    let rec _map spec =
      let user_file path name =
        let open Configuration_Map in
        let open Success.Infix in
        Success.map2
          merge
          (Configuration_Getopts.query())
          (Configuration_Environment.query())
        >>= fun config ->
        Success.return(get config {
          of_string = (fun x -> x);
          path;
          name;
          default = "";
          description = "User configuration file";
          })
        >>= Configuration_File.parse true
      in
      match spec with
      | Empty -> Success.return Configuration_Map.empty
      | Command_line -> Configuration_Getopts.query ()
      | Environment -> Configuration_Environment.query ()
      | OptionalFile(name) -> Configuration_File.parse false name
      | ImportantFile(name) -> Configuration_File.parse true name
      | UserFile(path,name) -> user_file path name
      | Heredoc(doc) -> Configuration_File.heredoc doc
      | Alist(bindings) -> Success.return(Configuration_Map.from_alist bindings)
      | Merge(a,b) -> Success.map2 Configuration_Map.merge (_map a) (_map b)
      | Override(a,b) -> Success.map2 Configuration_Map.override (_map a) (_map b)
      | exception Failure(mesg) -> error "Failure: %s" mesg
      | exception exn -> error "%s" (Printexc.to_string exn)

    let init spec =
      let open Success.Infix in
      _map spec >>= fun config ->
      (_config := config; Success.return())

  end

  let run name usage description
      ?(notes = [])
      ?(configuration = Configuration.Empty)
      main =
    let open Success.Infix in
    let safemain rest =
      try Success.return(main rest)
      with
      | Failure(mesg) ->
          if String.length mesg >= 7 && String.sub mesg 0 7 = "Usage: " then
            error_usage "%s" (String.sub mesg 7 (String.length mesg - 7))
          else
            error "Failure: %s" mesg
      | exn -> error "%s" (Printexc.to_string exn)
    in
    let program =
      (Configuration_Getopts.init usage description notes;
       Configuration.init configuration)
      >>= Component.bootstrap
      >>= fun () ->
      (Success.catch
         (Configuration_Getopts.rest() >>= safemain)
         (fun error -> Component.shutdown () >>= fun () -> Success.throw error))
      >>= Component.shutdown
    in
    let open Gasoline_SysExits in
    let open Success in
    match Success.run program with
    | Success() -> exit EXIT_SUCCESS
    | Error(Bootstrap(_, _))
    | Error(Shutdown(_, _))
    | Error(Software) ->
        exit EXIT_SOFTWARE
    | Error(Usage) ->
        exit EXIT_USAGE
end
