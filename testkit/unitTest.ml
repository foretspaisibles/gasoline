(* UnitTest -- Unit testing

Author: Michael Grünewald
Date: Mon Apr 14 23:40:15 CEST 2014

Gasoline (https://github.com/michipili/gasoline)
This file is part of Gasoline

Copyright © 2014 Michael Grünewald

This file must be used under the terms of the CeCILL-B.
This source file is licensed as described in the file COPYING, which
you should have received as part of this distribution. The terms
are also available at
http://www.cecill.info/licences/Licence_CeCILL-B_V1-en.txt *)

open Printf

(* Timestamp *)
let timestamp s =
  let open Unix in
  let day_of_week n =
    match n with
    | 0 -> "Sun"
    | 1 -> "Mon"
    | 2 -> "Tue"
    | 3 -> "Wed"
    | 4 -> "Thu"
    | 5 -> "Fri"
    | 6 -> "Sat"
    | _ -> invalid_arg "day_of_week"
  in
  let month_of_year n =
    match n with
    | 0 -> "Jan"
    | 1 -> "Feb"
    | 2 -> "Mar"
    | 3 -> "Apr"
    | 4 -> "May"
    | 5 -> "Jun"
    | 6 -> "Jul"
    | 7 -> "Aug"
    | 8 -> "Sep"
    | 9 -> "Oct"
    | 10 -> "Nov"
    | 11 -> "Dec"
    | _ -> invalid_arg "month_of_year"
  in
  sprintf "%s %s %2d %02d:%02d:%02d %04d"
    (day_of_week s.tm_wday)
    (month_of_year s.tm_mon)
    s.tm_mday
    s.tm_hour
    s.tm_min
    s.tm_sec
    (1900 + s.tm_year)

let curr_timestamp () =
  timestamp (Unix.gmtime (Unix.time()))

(* Misc *)

let expected_sz = 42	(* Expected size of our set of test cases *)

let path_cat p1 p2 =	(* Concatenation of path elements for test names *)
  if p1 = "" then
    p2
  else
    p1 ^ "." ^ p2

let equal_float ?(epsilon = epsilon_float) x y =
  abs_float (x -. y) <= epsilon *. (abs_float (max x y))


(* Test case *)

type t = {
  ident: string;
  predicate: unit -> bool
}

(* Creating tests *)

let assert_success ident = {
  ident;
  predicate = fun _ -> true
}

let assert_failure ident = {
  ident;
  predicate = fun _ -> false
}

let assert_true ident f x  = {
  ident;
  predicate = fun _ -> f x;
}

let assert_false ident f x = {
  ident;
  predicate = fun _ -> not (f x);
}

let assert_for_all ident p l = {
  ident;
  predicate = fun _ -> List.for_all p l;
}

let assert_exists ident p l = {
  ident;
  predicate = fun _ -> List.exists p l;
}

let assert_equal ident ?(equal = (=)) f x y = {
  ident;
  predicate = fun _ -> equal y (f x)
}

let assert_zero ident f x =
  assert_equal ident f x 0

let assert_nonzero ident f x =
  assert_equal ~equal:(!=) ident f x 0	(* Yes, it is perverse. *)

let assert_exception ident e f x = {
  ident;
  predicate = fun _ ->
    try ignore(f x); false
    with except when except = e -> true
}

let assert_float ident y f x =
  assert_equal ~equal:equal_float ident y f x

let float_to_precise_string n x =
  sprintf "%.*f" n x

let equal_precision n x y =
  (float_to_precise_string n x) = (float_to_precise_string n y)

let assert_precision label n f x y =
  assert_equal ~equal:(equal_precision n) label f x y

(* Compound test cases *)

let case_prepend prefix t = {
  t with ident = prefix ^ "." ^ t.ident
}

let case_combinator loop prefix list = {
  ident = prefix;
  predicate = fun _ ->
    loop (fun t -> t.predicate()) (List.map (case_prepend prefix) list);
}

let exists =
  case_combinator List.exists

let for_all =
  case_combinator List.for_all

let ident t =
  t.ident


(* Test fixtures *)

type fixture = {
  setup: unit -> unit;
  tear_down: unit -> unit;
}

let make_fixture setup tear_down = {
  setup;
  tear_down;
}

let bracket setup f x tear_down =
  let _ = setup () in
  let a = f x in
  let _ = tear_down () in
    a

let apply_fixture fixture f x =
  bracket fixture.setup f x fixture.tear_down


let donada =
  make_fixture ignore ignore

let pr_generator =
  Random.State.make_self_init ()

let pr_name prefix suffix =
  let pr_number = (Random.State.bits pr_generator) land 0xFFFFFF in
  let pr_basename = (sprintf "%s%06x%s" prefix pr_number suffix) in
    Filename.concat Filename.temp_dir_name pr_basename

let tmpfile prefix suffix r =
  let sname = ref "\000" in	(* An invalid filename *)
  let setup () =
    begin
      sname := Filename.temp_file prefix suffix;
      r := !sname
    end
  in
  let tear_down () =
    Unix.unlink !sname
  in { setup; tear_down }

let tmpdir_setup scwd sname prefix suffix r () =
  let tmpdir_max = 100 in	(* Max attempts *)
  let rec tmpdir_try counter =
    let name = pr_name prefix suffix in
      try begin
	Unix.mkdir name 0o700;
	r := name;
	sname := name;
	scwd := Unix.getcwd();
	try Unix.chdir name
	with Sys_error _ as e -> begin
	  Unix.chdir !scwd;
	  raise e
	end
      end with Sys_error _ as e ->
	if counter >= tmpdir_max then raise e else tmpdir_try (counter + 1)
  in tmpdir_try 0

let rec rmRf path =
  let dir = Unix.opendir path in
    try while true do
      let base = Unix.readdir dir in
      let full = Filename.concat path base in
      let stat = Unix.lstat full in
	match base, stat.Unix.st_kind with
	| ".", _
	| "..", _ -> ()
	| _, Unix.S_REG
	| _, Unix.S_LNK
	| _, Unix.S_FIFO
	| _, Unix.S_SOCK -> Unix.unlink full
	| _, Unix.S_DIR -> rmRf full
	| _, Unix.S_CHR
	| _, Unix.S_BLK -> ()
    done with End_of_file -> begin
      Unix.closedir dir;
      Unix.rmdir path
    end

let tmpdir_tear_down scwd sname () =
  begin
    Unix.chdir !scwd;
    rmRf !sname;
  end

let tmpdir prefix suffix reference =
  let scwd = ref "\000" in	(* An invalid filename *)
  let sname = ref "\000" in	(* An invalid filename *)
    {
      setup = tmpdir_setup scwd sname prefix suffix reference;
      tear_down = tmpdir_tear_down scwd sname;
    }




(* Outcome *)

type outcome =
  | Success
  | Failure
  | Not_implemented
  | Skipped
  | Exception of exn

let outcome_to_char = function
  | Success -> ' '
  | Failure -> '~'
  | Not_implemented -> '?'
  | Skipped -> '>'
  | Exception _ -> '!'

let outcome_to_string ident outcome =
  let c = outcome_to_char outcome in
  match outcome with
    | Exception exn -> sprintf "%c %s %s" c ident (Printexc.to_string exn)
    | _ -> sprintf "%c %s" c ident

let outcome_describe = function
  | Success -> "succeed"
  | Failure -> "failed"
  | Not_implemented -> "not implemented"
  | Skipped -> "skept"
  | Exception exc ->
      sprintf "raised %s" (Printexc.to_string exc)

let outcome_is_successful = function
  | Success
  | Skipped -> true
  | _ -> false


(* Test suites *)

type suite_item =
  | Case of fixture * t
  | Suite of fixture * suite
and suite = {
  suite_ident: string;
  suite_fixture: fixture;
  suite_queue: suite_item Queue.t;
  mutable suite_init: (suite -> unit) option;
}

let make ?(fixture = donada) ?init ident = {
  suite_ident = ident;
  suite_fixture = fixture;
  suite_queue = Queue.create();
  suite_init = init;
}

let add_case ?(fixture = donada) s case =
  Queue.add (Case(fixture, case)) s.suite_queue

let add_suite ?(fixture = donada) s suite =
  Queue.add (Suite(fixture, suite)) s.suite_queue

let suite_maybe_init s =
  match s.suite_init with
  | Some callback -> (callback s; s.suite_init <- None)
  | None -> ()

(* Supervisor *)

type message =
  | NOT_IMPLEMENTED
  | SKIP

exception Message of message

let message m =
  raise(Message(m))

let not_implemented () =
  message NOT_IMPLEMENTED

let skip_if p =
  if p then
    message SKIP

let only_for p =
  skip_if (not p)

let run_case t =
  (* The outcome of a test case *)
  try if t.predicate ()
  then
    Success
  else
    Failure
  with
    | Message(NOT_IMPLEMENTED) -> Not_implemented
    | Message(SKIP) -> Skipped
    | other -> Exception other


class type supervisor =
object
  method case_begin : string -> t -> unit
  method case_end : string -> t -> unit
  method case_outcome : string -> t -> outcome -> unit
  method case_run : string -> fixture -> t -> bool
  method root_begin : string -> unit -> unit
  method root_end : string -> unit -> unit
  method root_run : suite -> bool
  method suite_begin : string -> suite -> unit
  method suite_end : string -> suite -> unit
  method suite_run : string -> fixture -> suite -> bool
  method summary : unit -> unit
end

class virtual meta_supervisor =
  let root_memoize = Hashtbl.create expected_sz in
object(self)

  method virtual root_begin : string -> unit -> unit
  method virtual root_end : string -> unit -> unit
  method virtual suite_begin : string -> suite -> unit
  method virtual suite_end : string -> suite -> unit
  method virtual case_begin : string -> t -> unit
  method virtual case_outcome : string -> t -> outcome -> unit
  method virtual case_end : string -> t -> unit
  method virtual summary : unit -> unit

  method private really_suite_run ident setup s tear_down =
    let is_true b = b in
    let _ = suite_maybe_init s in
    let _ = s.suite_fixture.setup () in
    let _ = setup () in
    let l = List.rev (Queue.fold (fun a x -> x :: a) [] s.suite_queue) in
    let x = List.map (self#suite_item_run ident) l in
    let a = List.for_all is_true x in
    let _ = tear_down () in
    let _ = s.suite_fixture.tear_down() in
      a

  method private really_root_run s =
    let n = s.suite_ident in
    let a = self#really_suite_run n (self#root_begin n) s (self#root_end n)
    in (self#summary (); a)

  method suite_run ident f s =
    let () = self#suite_begin ident s in
    let answer =
      self#really_suite_run (path_cat ident s.suite_ident)
	f.setup s f.tear_down
    in
    let () = self#suite_end ident s in
    answer

  method root_run s =
    try Hashtbl.find root_memoize s.suite_ident
    with Not_found -> self#really_root_run s

  method private suite_item_run ident item =
    match item with
      | Case(f,c) -> self#case_run ident f c
      | Suite(f,s) -> self#suite_run ident f s

  method case_run ident fixture c =
    let path = path_cat ident c.ident in
    let _ = self#case_begin path c in
    let o = apply_fixture fixture run_case c in
    let _ = self#case_outcome path c o in
    let _ = self#case_end path c in
      outcome_is_successful o

end

class outcome_memoizer = (* Keeping track of outcomes *)
  let outcome_memoize : (string * outcome) Queue.t = Queue.create () in
object(self)
  method reset =
    Queue.clear outcome_memoize
  method push ident outcome =
    Queue.add (ident, outcome) outcome_memoize
  method get =
    List.rev (Queue.fold (fun a x -> x :: a) [] outcome_memoize)
end

class verbose_supervisor : supervisor =
  let memo = new outcome_memoizer in
object
  inherit meta_supervisor

  method root_begin ident () =
    memo#reset;
    printf "===>  Begin test suite %s\n" ident

  method root_end ident () =
    printf "===>  End test suite %s\n" ident

  method suite_begin _ _ =
    ()

  method suite_end _ _ =
    ()

  method case_begin ident case =
    printf "=>  Test case %s\n" ident

  method case_outcome ident case outcome =
    memo#push (path_cat ident case.ident) outcome;
    printf "=> Test case %s %s\n" ident (outcome_describe outcome)

  method case_end ident case =
    ()

  method summary () =
    flush stdout;
    List.iter (fun (i,o) -> prerr_endline (outcome_to_string i o)) memo#get

end


class concise_supervisor : supervisor =
  let memo = new outcome_memoizer in
  let sep = String.make 70 '-' in
  let prerr_sep () =
    prerr_endline sep
  in
object(self)
  inherit meta_supervisor

  val fd_stdout = Unix.dup Unix.stdout
  val fd_stderr = Unix.dup Unix.stderr

  val stat_root_start = ref 0.
  val stat_root_stop = ref 0.
  val stat_case_count = ref 0

  method private logfile_begin file =
    let logfile = open_out file in
    begin
      List.iter flush [
	stdout;
	stderr;
      ];
      Unix.dup2 (Unix.descr_of_out_channel logfile) Unix.stdout;
      Unix.dup2 fd_stdout (Unix.stderr);
      close_out logfile;
    end

  method private logfile_end () =
    begin
      List.iter flush [
	stdout;
	stderr;
      ];
      Unix.dup2 fd_stdout (Unix.stdout); (* Closes logfile bound to stdout *)
      Unix.dup2 fd_stderr (Unix.stderr);
    end

  method root_begin ident () =
    begin
      stat_root_start := Sys.time();
      stat_case_count := 0;
      memo#reset;
      self#logfile_begin (ident ^ ".log");
      eprintf "Test suite %s\n" ident;
    end

  method root_end ident () =
    begin
      stat_root_stop := Sys.time();
      prerr_newline ();
      self#logfile_end ();
    end

  method suite_begin _ _ =
    ()

  method suite_end _ _ =
    ()

  method case_begin ident case =
    incr stat_case_count;
    printf "From UNIT-TEST %s\n" (curr_timestamp());
    printf "Test-Case: %s\n" ident;

  method case_outcome ident case outcome =
    let outcome_brief =
      match outcome_to_char outcome with
      | ' '-> '.'
      | x -> x
    in
    printf "Test-Outcome-Brief: %c\n" outcome_brief;
    printf "Test-Outcome: %s\n" (outcome_describe outcome);
    printf "\n";
    memo#push (path_cat ident case.ident) outcome;
    prerr_char outcome_brief

  method case_end ident case =
    printf "\n"

  method summary () =
    let prerr_outcome (i,o) =
      if not (outcome_is_successful o) then
	prerr_endline (outcome_to_string i o)
    in begin
	flush stdout;
	flush stderr;
	prerr_sep();
	eprintf "Ran %d tests in %.3fs\n\n%!" !stat_case_count
	  (!stat_root_stop -. !stat_root_start);
	List.iter prerr_outcome memo#get
      end

end

let verbose = new verbose_supervisor
let concise = new concise_supervisor


(* Root suite registry *)

type root = {
  root_suite: suite;
  root_prerequisite: string list;
}

let root_registry =
  Hashtbl.create expected_sz

let mem =
  Hashtbl.mem root_registry


let register ?(prerequisite = []) s =
  if List.for_all mem prerequisite then
    Hashtbl.add root_registry s.suite_ident {
      root_suite = s;
      root_prerequisite = prerequisite;
    }
  else failwith "unitary test: unknown dependency"

let with_registered_suite ?fixture ?prerequisite name init =
  let suite = make ?fixture ~init name in
  register ?prerequisite suite

let package ?(prerequisite = []) (name, l) =
  let getsuite x =
    let r =
      try Hashtbl.find root_registry x
      with Not_found -> failwith(sprintf "%s: unknown test suite" x)
    in
    if r.root_prerequisite <> [] then
      failwith(sprintf "%s: has prerequisites" x)
    else
      r.root_suite
  in
  let subsuite =
    List.map getsuite l
  in
  let suite = make name in
  List.iter (add_suite suite) subsuite;
  List.iter (Hashtbl.remove root_registry) l;
  register ~prerequisite suite


module Ident = Set.Make(String)

let set_of_list l =
  List.fold_right Ident.add l Ident.empty

let set_to_list s =
  Ident.fold (fun h t -> h :: t) s []

let list () =
  let loop ident _ a = Ident.add ident a in
  let sort = List.sort String.compare in
  let root_registry_keys = Hashtbl.fold loop root_registry Ident.empty in
  sort(set_to_list root_registry_keys)

let rec toposort
    n	(* Number of elements in u at the beginning of the last iteration *)
    l	(* List of sorted identifiers *)
    a	(* Set of sorted identifiers, it has the same elements as l *)
    u	(* Dependencies to go *)
    v	(* Postponed dependencies *)
    =
  match u with
    | [] when v = [] -> l
    | [] when List.length v = n -> failwith "toposort"
    | [] -> toposort (List.length v) l a v []
    | (ident, deps) :: t when Ident.subset deps a ->
      toposort n (ident::l) (Ident.add ident a) t v
    | h :: t -> toposort n l a t (h :: v)

let direct_dependencies ident =
  let deps r = set_of_list r.root_prerequisite in
  let l = Hashtbl.find_all root_registry ident in
  List.fold_left Ident.union Ident.empty (List.map deps l)

let rec deep_dependencies
    a	(* Final dependencies *)
    b	(* Dependencies to explore *)
    =
  if Ident.subset b a then
    a
  else
    let x = Ident.choose b in
    let d = direct_dependencies x in
    deep_dependencies (deep_dependencies a d) (Ident.remove x b)

let dependencies ident =
  set_to_list (deep_dependencies Ident.empty (Ident.add ident Ident.empty))

let rec run ?(supervisor = concise) ident =
  let supervise r =
    supervisor#root_run r.root_suite
  in
    if mem ident then
      (List.for_all run (dependencies ident))
      && (List.for_all supervise (Hashtbl.find_all root_registry ident))
    else
      failwith (sprintf "test suite not found: %s" ident)

let run_several ?supervisor list =
  let is_true x = x in
  (* Recall that List.for_all is a short-cut and operator *)
  List.for_all is_true (List.map (run ?supervisor) list)

let run_all ?supervisor () =
  run_several ?supervisor (list())



let bracket setup test tearoff = {
  test with predicate = fun () ->
    begin
      setup ();
      let answer = test.predicate () in
      tearoff();
      answer
    end
}

let prerr_usage () =
  let progname = (Filename.basename Sys.executable_name) in
  eprintf "\
Usage: %s [-h | -l | suite1 [suite2 [...]]]
 Run unitary tests
Options:
 -h Display a cheerful help message.
 -l List available primary test suites.
Exit Status:
 The %s program exits 0 on success and 1 if a test case failed.
" progname progname

let help () =
  prerr_usage();
  exit 0

let usage () =
  prerr_usage();
  exit 64	(* Cf. sysexits(3) *)

let main () =
  if Array.length Sys.argv <= 1 then
    exit (if run_all () then 0 else 1)
  else if Sys.argv.(1) = "-h" then
    help ()
  else if Sys.argv.(1) = "-l" then begin
    List.iter print_endline (list ());
    exit 0
  end else
    exit (if run_several (List.tl (Array.to_list Sys.argv)) then 0 else 1)
