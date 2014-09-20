(* Getopt -- Program arguments analysis

Author: Michael Grünewald
Date: Sun  4 May 2008 11:08:10 CEST

Copyright © 2008-2012 Michael Grünewald

This file must be used under the terms of the CeCILL-B.
This source file is licensed as described in the file COPYING, which
you should have received as part of this distribution. The terms
are also available at
http://www.cecill.info/licences/Licence_CeCILL-B_V1-en.txt *)
(** Processing the command line

The [Getopt] module is used to break up options in command lines and
interpret them.  It supports the canonical UNIX convention, long
options and long arguments. *)

(** {1 How program arguments are read}

In this module, the argument vector passed to the process by {i
execve(2)} is broken up into the following pieces: the executable
name, the sequence of flags and options, then the sequence of main
arguments.

Instead of giving a formal definition of the rules used to analyze the
command line, we illustrate it agains a few examples. We assume that
the program accepts the flags ['h'], ['v'] and ['a'], and the options
['o'] and ['m']. Now let us see how some arguments vectors are broken
up into pieces:

- [program -v -o output input] is understood as {i the flag v },
  {i the option o with the argument "output"} and
  {i the rest argument "input"};
- [program -vo output input] and [program -vooutput input] are analyzed the
  same way, since flag clustering is allowed;
- [program -ovoutput input] is understood as {i the option o with the
  argument "voutput"} and {i the main argument "input"}.

Some programs, as {i gcc} or {i clang}, use a flag to introduce {i
key/value} style options, as in
[clang -march=CPU -mmacosx-version-min=10.6].  This scheme is also
supported by this module, see {i long options}.

While the procedures defined in this module allows a program to deal
with its argument as most of the UNIX programs do, there is many
schemes not supported by it. Among these are
- tar style, where flags and options are grouped together and the
  arguments come later;
- sox style, where options affect only the file preceding them;
- svn style, where the svn program multiplexes many programs in one. *)

(** {6 Traditional flags and options} *)

type t
(** {e That getopt thingie} [t] represents a flag or an option.

Values of this type are created by the functions
[flag], [char], [bool], [string], [int] and [float] hereafter, their
variants [set_flag], [set_char], [set_bool], [set_string], [set_int]
and [set_float], and the [long] forging long options. *)

type 'a reader = string -> 'a
(** The type of functions converting [string] to ['a].  Such a
function can raise [Failure] (preferred) or [Invalid_arg] to indicate
an ill-formed input. *)

val flag : char -> (unit -> unit) -> string -> t
(** [char c f d] constructs a [t] that handles the flag denoted by
[c]. When this flag occurs in the command line, the callback [f] is
called. The message [d] may be displayed in the
help screen of the program. *)

val char : char -> (char -> unit) -> string -> t
(** [char c f d] constructs a [t] that handles the option denoted by
[c]. This options has a [char] value as argument. When this option
occurs in the command line, the callback [f] is called on the option
argument. The message [d] may be displayed in the help screen of the
program. *)

val bool : char -> (bool -> unit) -> string -> t
(** Similar to [char] for boolean values. *)

val string : char -> (string -> unit) -> string -> t
(** Similar to [char] for string values. *)

val int : char -> (int -> unit) -> string -> t
(** Similar to [char] for integer values. *)

val float : char -> (float -> unit) -> string -> t
(** Similar to [char] for floating point values. *)

val concrete : 'a reader -> char -> ('a -> unit) -> string -> t
(** [concrete conc c f d] constructs a [t] that handles the option
denoted by [c]. This options has a ['a] value as argument converted
from the given string by [conc].  When this option occurs in the
command line, the callback [f] is called on the option argument. The
message [d] may be displayed in the help screen of the program. *)


(** {6 Help notes} *)

type note
(** A [note] is a message that shall be displayed when the user asks
for the help screen of the program. Such a message has a title and a
body, which is broken down into paragraphs. *)

val note : string -> string -> note
(** [note title body] constructs a [note]. In the [body], paragraphs
are separated by end-of-line characters. *)


(** {6 Command line processing specifications} *)

type spec
(** The abstract type of command line processing specifications.

Such a specification is made up of flags and notes. *)

val spec :
  string -> string -> t list -> (string -> unit) -> note list -> spec
(** [spec usage description options rest notes] builds an environment
for command line analysis. *)


(** {6 Help screen} *)

val help : spec -> unit
(** Output the help screen associated to the analysis specification
and terminate the application. *)

val usage : spec -> string -> unit
(** Output a message and the usage associated to the analysis specification
and terminate the application with exit code 64. *)


(** {6 Parse command options} *)

val parse : spec -> string array -> unit
(** [parse] uses an environment to analyse an argument vector. *)

val parse_argv : spec -> unit
(** This is a specialisation of [parse] to the process arguments
vector. *)


(** {6 Long options} *)

type long_option
(** The type of long options.  Several long options can be packed
together to be multiplexed by a traditional short option. *)

val long : char -> long_option list -> string -> t
(** [long flag longopt_list description] constructs a short option
multiplexing a load of long options. *)

val long_flag : string -> (unit -> unit) -> long_option
(** [long_flag name cb] create a long option flag named [name]
triggering the callback [cb]. *)

val long_char : string -> (char -> unit) -> long_option
(** [long_flag name cb] create a long optionan argument named [name]
requiring a character argument and triggering the callback [cb]. *)

val long_bool : string -> (bool -> unit) -> long_option
(** Similar to [long_char] for boolean values. *)

val long_string : string -> (string -> unit) -> long_option
(** Similar to [long_char] for string values. *)

val long_int : string -> (int -> unit) -> long_option
(** Similar to [long_char] for integer values. *)

val long_float : string -> (float -> unit) -> long_option
(** Similar to [long_char] for floating point values. *)

val long_concrete : 'a reader -> string -> ('a -> unit) -> long_option
(** Similar to [long_char] for arbitrary values. *)


(** {6 Creating callbacks} *)

val store : 'a ref -> 'a -> unit
(** [store r v] is the same as [r := v] and can be partially evaluated
to form a callback. *)

val set : 'a -> 'a ref -> unit -> unit
(** [set v r ()] is the same as [r := v] and and can be partially evaluated
to form a callback. *)

val queue : 'a list ref -> 'a -> unit
(** [queue a v] add [v] at the end of [!a]. It can be partially
evaluated to form a callback. *)
