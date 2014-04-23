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


(** Unit testing.

This {i unit test framework} supports {i test automation, sharing
of setup and teardown code} for tests and {i aggregation of tests in
collections}.

To achieve this, we support some important concepts:

{b Test case.}
A test case is the smallest unit of testing.  It usually compares an
expected behaviour with the actual behaviour for a particular set of
inputs.


{b Test outcome.}
A test outcome represents the result of a test case, {i i.e.} if the
actual behaviour matches the expected behaviour.


{b Test fixture.}
A test fixture represents the preparation needed to perform one or
more test cases, and any associated cleanup actions.  This may
involve, for example, creating temporary directories, logging into a
database, or starting a server process.


{b Test suite.}
A test suite is a collection of test cases, test suites, or
both.  It is used to aggregate tests that should be executed
together, for instance, the tests for a given component.


{b Test supervisor.}
A test supervisor is a component which orchestrates the execution of
tests and provides the outcome to the user.  The supervisor may use a
graphical interface, a textual interface, or return a special value to
indicate the results of executing the tests. *)


type t
(** The abstract type of test cases. *)


type 'a printer = Format.formatter -> 'a -> unit
(** The type of printers for values of type ['a]. *)

(** {6 Simple test cases} *)

val assert_success: string -> t
(** A test case that always succeeds, the string identifies the test case. *)

val assert_failure: string -> t
(** A test case that always fails, the string identifies the test case. *)

val assert_equal: string ->
  ?printer:('b printer) ->
  ?equal:('b -> 'b -> bool) -> ('a -> 'b) -> 'a -> 'b -> t
(** [assert_equal ident f x y] creates a test case computing [f x] and comparing
the returned value with [y].  The test case succeeds if these values
are equal and fails otherwise.

Diagnostic messages can be emitted to [stdout], they are gathered
by the test supervisor orchestrating the execution of the test case.
Use of the [Format] module is encouraged.

@param equal Predicate used to compare values (defaults to polymorphic [=]).
@param printer Printer function used to output values.
*)

val assert_true: string -> ('a -> bool) -> 'a -> t
(** Specialised version of [assert_equal] for [true]. *)

val assert_false: string -> ('a -> bool) -> 'a -> t
(** Specialised version of [assert_equal] for [false]. *)

val assert_for_all: string ->
  ?printer:('a printer) ->
  ('a -> bool) -> 'a list -> t
(** [assert_for_all ident predicate list] creates a test case
verifying that all the elements of [list] pass the predicate.

@param printer Printer function used to output values. *)

val assert_exists: string ->
  ?printer:('a printer) ->
  ('a -> bool) -> 'a list -> t
(** [assert_for_all ident predicate list] creates a test case
verifying that at least one element of [list] passes the predicate.

@param printer Printer function used to output values. *)

val assert_zero: string -> ('a -> int) -> 'a -> t
(** Specialised version of [assert_equal] for [0]. *)

val assert_nonzero: string -> ('a -> int) -> 'a -> t
(** A test case similar to [assert_equal], testing for a non zero result. *)

val assert_float: string -> ('a -> float) -> 'a -> float -> t
(** A test case similar to [assert_equal], testing for a floating
point result being close from an expected value. *)

val assert_precision: string -> int -> ('a -> float) -> 'a -> float -> t
(** A test case similar to [assert_float], testing for a floating
point result being close from an expected value with a given
precision. *)

val assert_exception: string -> exn -> ('a -> 'b) -> 'a -> t
(** A test case that succeeds only when the computed function raises the
given exception. *)


(** {6 Compound test cases} *)

val for_all: string -> t list -> t
(** A test case succeeding only if all the tests in the given list do.
The string identifies the test case. *)

val exists: string -> t list -> t
(** A test case succeeding only if one of the tests in the given list does.
The string identifies the test case. *)


(** {6 Examining test cases} *)

val ident: t -> string
(** The string identifying a test case. *)


(** {6 Test fixtures} *)

type fixture
(** The abstract type of test fixtures. *)

val make_fixture : (unit -> unit) -> (unit -> unit) -> fixture
(** [make_fixture setup tear_down] creates a test fixture running [setup]
before a test and [tear_down] after its complextion, successful or
not. *)

val relax : fixture
(** A trivial test fixture, doing nothing. *)

val tmpfile : string -> string -> string ref -> fixture
(** A test fixture creating a temporary file before running the test and
deleting it after test completion.

[tmpfile prefix suffix r] creates a temporary file whose name is
derivated from the concatenation of [prefix], some appropriate
random number and a [suffix].  This name is stored in the
reference [r].

The temporary file is created empty, with permissions [0o600]
(readable and writable only by the file owner). The file is
guaranteed to be different from any other file that existed when
[tmpfile] was called.

@raise Sys_error if the file could not be created. *)

val tmpdir : string -> string -> string ref -> fixture
(** A test fixture creating a temporary directory and making it the current
working directory before running the test and moving back to the previous
current working directory then deleting the temporary directory after test
completion. *)


(** {6 Test suites} *)

type suite
(** The abstract type of test suites. *)

val make : ?fixture:fixture -> ?init:(suite -> unit) -> string -> suite
(** Create a test suite, identified by the string.  If given, the
fixture applies to the test suite as a whole. *)

val add_case : ?fixture:fixture -> suite -> t -> unit
(** Add a test case to a test suite. *)

val add_suite : ?fixture:fixture -> suite -> suite -> unit
(** Add a test suite to a test suite. *)


(** {6 Supervisors} *)

type supervisor
(** The abstract type of test supervisors. *)

val verbose : supervisor
(** A simple supervisor printing a lot of details on stdout and
terminating with a summary on stderr. *)

val concise : supervisor
(** A simple supervisor printing only details pertaining to test
failures on stderr.

A log file is prepared from the standard output of the given
test suite. *)

val not_implemented: unit -> unit
(** Used by test case code to signal its supervisor that a feature is
not yet implemented. *)

val skip_if: bool -> unit
(** Used by test case code to signal its supervisor that a test should
not be performed if the given condition is met. *)

val only_for: bool -> unit
(** Used by test case code to signal its supervisor that a test should
only be performed when the given condition is met.

This is equivalent to [skip_if] with a negated condition. *)


(** {6 Managing tests} *)

val register: ?prerequisite:string list -> suite -> unit
(** Register a test suite.

@param prerequisiste lists the identifiers of test cases whose
successful completion conditions the sucessful completion of the
registered test.

For instance if [A] uses [B] and [B] fails, the results for the
testing of [A] are meaningless, so the tests for [A] have to be
skept. *)

val with_registered_suite : ?fixture:fixture -> ?prerequisite:string list -> string -> (suite -> unit) -> unit
(** [with_registered_suite name init] apply [init] on a fresh new,
registered suite, called [name].

Actually, [init] is only called when the suite runs for the first time.

@raise Failure if some prerequisite is not registered. *)

val package : ?prerequisite:string list -> (string * (string list)) -> unit
(** [package (name,subsuite)] create a test suite called [name]
containing the test suites listed in [subsuite].

@raise Failure is one of the subsuite is not known. *)


val list: unit -> string list
(** List of registered test suites. *)

val run: ?supervisor:supervisor -> string -> bool
(** Run the given test suite. The return value indicates the success
of the test suites, that is, if each test either returned
[Success] or [Skipped]. *)

val run_all: ?supervisor:supervisor -> unit -> bool
(** Run all available test suites. *)

val main: unit -> unit
(** Main procedure for unitary tests.  It analyses the command line
and performs the appropriate actions. *)
