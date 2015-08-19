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
  | Bootstrap of string * string * (string list)
  (* Bootstrap(mesg, name, lst) signals an error while bootstrapping
     the component [name]. The list [lst] enumerates the components
     which have been correctly bootstrapped. *)

  | Shutdown of string * string * (string list)
  (* Shutdown(mesg, name, lst) signals an error while shutting down
     the component [name]. The list [lst] enumerates the components
     which still need to be shut down. *)

  | Software of string
  | Usage of string
  | Getopts of string

let ( $ ) f g =
  fun x -> f (g x)

module Success =
  Lemonade_Success.Make(struct type t = error end)

module Maybe =
  Lemonade_Maybe

type validate =
  (string -> string -> string -> string Success.t) -> string Success.t Configuration_Map.key

let progname () =
  Filename.basename Sys.executable_name

let die code argv =
  Printf.fprintf stderr "%s: " (progname());
  Printf.kfprintf
    (fun outc -> output_char outc '\n'; Gasoline_SysExits.exit code) stderr
    argv

let wlog argv =
  Printf.fprintf stderr "%s: " (progname());
  Printf.kfprintf
    (fun outc -> output_char outc '\n') stderr
    argv

let error argv =
  Printf.ksprintf (fun s -> Success.throw (Software s)) argv

let error_bootstrap name lst argv =
  Printf.ksprintf (fun s -> Success.throw(Bootstrap(s, name, lst))) argv

let error_shutdown name lst argv =
  Printf.ksprintf (fun s -> Success.throw(Shutdown(s, name, lst))) argv

let error_usage argv =
  Printf.ksprintf (fun s -> Success.throw(Usage(s))) argv

let error_getopts argv =
  Printf.ksprintf (fun s -> Success.throw(Getopts(s))) argv

(* Rules to sort the component list.

   1. If two independent components provide the same thing, then both
   component must be executed to meet the barrier.

   2. If two interdependent components provide the same thing, then
   both component must be executed in graph order to meet the
   barrier.

   3. We pick a component, stack it as “in progress” and try to
   satisfy all its requirements. Once all of an in-progress script
   are met, we can remove it from the “in progress” stack. *)

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
  type 'a kind
  type t
  val make : 'a kind -> 'a -> t
  val to_string : t -> string
  val of_string : string -> t
  val of_string_kind : 'a kind -> string -> 'a
  val kind_name : 'a kind -> string
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
    type 'a kind
    type component =
      Component.t

    val make : 'a kind -> component ->
      ?flag:char -> ?env:string -> ?shy:bool ->
      string -> 'a -> string -> (unit -> 'a)

    type spec =
    | Empty
    | Command_line
    | Environment
    | File of string
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

    let _graph table =
      let require comp =
        List.map (fun r -> (comp.name, r)) comp.require
      in
      let provide comp =
        List.map (fun p -> (p, comp.name)) comp.provide
      in
      let edges table =
        let loop _ comp ax =
          ax
          |> List.fold_right EdgeSet.add (require comp)
          |> List.fold_right EdgeSet.add (provide comp)
        in
        Hashtbl.fold loop table EdgeSet.empty
        |> EdgeSet.filter (fun edge -> fst edge <> snd edge)
        |> EdgeSet.elements
      in
      let nodes table =
        let loop _ comp ax =
          comp.name :: ax
        in
        Hashtbl.fold loop table []
      in
      let select edges node =
        (node, List.map snd (List.filter (fun (x,_) -> x = node) edges))
      in
      List.map (select (edges table)) (nodes table)

    let graph () =
      Success.return(_graph _component_table)

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
        | Failure(mesg) -> error_bootstrap comp.name acc "%s" mesg
        | exn -> error_bootstrap comp.name acc "%s" (Printexc.to_string exn)
      in
      graph ()
      >>= toposort
      >>= (Success.return $ List.rev)
      >>= process f []
      >>= (Success.return $ ignore)

    let shutdown () =
      let open Success.Infix in
      let f comp acc =
        try (comp.shutdown (); Success.return(List.filter ((<>) comp.name) acc))
        with
        | Failure(mesg) -> error_shutdown comp.name acc "%s" mesg
        | exn -> error_shutdown comp.name acc "%s" (Printexc.to_string exn)
      in
      let rec loop lst =
        Success.catch
          (process f lst lst)
          (function
            | Shutdown(name, mesg, pending) ->
                (wlog "%s: shutdown: %s" name mesg;
                 loop pending >>= fun _ -> error "An error occured during shutdown.")
            | whatever -> Success.throw whatever)
      in
      graph ()
      >>= toposort
      >>= loop
      >>= function
      | [] -> Success.return ()
      | whatever ->
          error
            "%s: Skept by the shutdown procedure."
            (String.concat ", " whatever)
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

    let query () =
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
  end

  (* Keep trace of command line options mapped to configurations. *)
  module Configuration_Getopts :
  sig
    val init : string -> string -> (string*string) list -> unit
    (* [add path name flag description] *)
    val add : validate -> char -> unit
    val query : unit -> Configuration_Map.t Success.t
    val rest : unit -> string list Success.t
  end = struct
    let _table = ref []
    let _context = ref None

    let spec () =
      let open Success.Infix in
      (match !_context with
       | Some(whatever) -> Success.return whatever
       | None -> error "Configuration_Getopt: Context not initialised.")
      >>= fun (usage, description, notes) ->
      Success.return
        (Getopts.spec usage description !_table
           (fun arg m ->
              (Success.map (fun (config, rest) -> (config, arg :: rest)) m))
           (List.map (fun (title, body) -> Getopts.note title body) notes))

    let init usage description notes =
      _context := Some(usage, description, notes)

    let add validatekey flag =
      let catch kindname text _ =
        error_getopts "Invalid Argument -- %c %S: Bad %s value."
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
        Getopts.option
          (fun s -> s)
          flag
          (fun optarg m ->
             (Success.map2 fold (Configuration_Map.value configkey optarg) m))
          configkey.Configuration_Map.description
      in
      _table := option :: !_table

    let _parse () =
      let open Success.Infix in
      spec () >>= fun s ->
      (Getopts.parse_argv s (Success.return (Configuration_Map.empty, [])))
      >>= fun (config, rest) -> Success.return(config, List.rev rest)

    let query () =
      let open Success.Infix in
      _parse () >>= (Success.return $ fst)

    let rest () =
      let open Success.Infix in
      _parse () >>= (Success.return $ snd)
  end

  module Configuration_File :
  sig
    (* [add path name env] *)
    val add : validate -> unit
    val parse : string -> Configuration_Map.t Success.t
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

    let parse name =
      (* Configuration values used in UserFile should be
           initialised to the empty string. *)
      let open Success.Infix in
      (if name <> "" then
         try Success.return(Configuration_Map.from_file name)
         with
         | Failure(mesg) -> error "Failure: %s" mesg
         | Sys_error(mesg) -> error "Error: %s" mesg
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
    type 'a kind =
      'a Parameter.kind

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

    let make kind comp ?flag ?env ?shy name default description =
      let component_path comp =
        comp.config_prefix @ [ comp.name ]
      in
      let concrete =
        Configuration_Map.{
          of_string =
            (fun text ->
               try Parameter.of_string_kind kind text
               with Failure(_) ->
                 Printf.ksprintf failwith "%s" (Parameter.kind_name kind));
          to_string = (fun x ->
              Parameter.to_string
                (Parameter.make kind x));
        }
      in
      let configkey =
        Configuration_Map.key
          concrete
          (component_path comp)
          name
          default
          description
      in
      let validate catch =
        Configuration_Map.{
          of_string =
            (fun text ->
               try (ignore(Parameter.of_string_kind kind text);
                    Success.return text)
               with Failure(mesg) -> catch (Parameter.kind_name kind) text mesg);
          to_string = (fun x ->
              match Success.run x
              with
              | Success.Success(s) -> s
              | whatever -> "<failure>");
        }
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
       | Some c -> Configuration_Getopts.add validatekey c
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
      | File of string
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
          concrete = {
            of_string = (fun x -> x);
            to_string = (fun x -> x);
          };
          path;
          name;
          default = "";
          description = "User configuration file";
          })
        >>= Configuration_File.parse
      in
      match spec with
      | Empty -> Success.return Configuration_Map.empty
      | Command_line -> Configuration_Getopts.query ()
      | Environment -> Configuration_Environment.query ()
      | File(name) -> Configuration_File.parse name
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
      | Failure(mesg) -> error "Failure: %s" mesg
      | exn -> error "%s" (Printexc.to_string exn)
    in
    let program =
      (Configuration_Getopts.init name usage notes;
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
    | Error(Bootstrap(mesg, name, _)) ->
        die EXIT_SOFTWARE "bootstrap: %s: %s" name mesg
    | Error(Shutdown(mesg, name, _)) ->
        die EXIT_SOFTWARE "shutdown: %s: %s" name mesg
    | Error(Software(mesg)) ->
        die EXIT_SOFTWARE "%s" mesg
    | Error(Usage(mesg)) ->
        die EXIT_USAGE "%s" mesg
    | Error(Getopts(mesg)) ->
        die EXIT_USAGE "%s\nUsage: %s %s" mesg (progname()) usage
end
