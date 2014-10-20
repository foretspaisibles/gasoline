(* CApplication -- C-Stylish Applications

Author: Michael Grünewald
Date: Sat Dec 28 13:05:16 CET 2013

Gasoline (https://github.com/michipili/gasoline)
This file is part of Gasoline

Copyright © 2013 Michael Grünewald

This file must be used under the terms of the CeCILL-B.
This source file is licensed as described in the file COPYING, which
you should have received as part of this distribution. The terms
are also available at
http://www.cecill.info/licences/Licence_CeCILL-B_V1-en.txt *)
(** C-Stylish Applications. *)

(** Localisation parameters. *)
module Locale :
sig

  (** The type of localisations parameters. We imitate the definitions
  of the C locale.h header file.

  The main differences with the C version are:
   - Decimal points and separator are strings and not chars.
   - Decimal grouping information is provided as a list,
     and 0 has a different meaning.

  Reference: http://pubs.opengroup.org/onlinepubs/009695399/basedefs/xbd_chap07.html#tag_07_03 *)
  type t = {

    decimal_point : string;
    (** Decimal-point separator used for non-monetary quantities. *)

    thousands_sep : string;
    (** Separators used to delimit groups of digits to the left of the
    decimal point for non-monetary quantities. *)

    grouping : int list;
    (** Specifies the amount of digits that form each of the groups to
    be separated by [thousands_sep] separator for non-monetary
    quantities.

    A mone indicate in the list indicates that no more grouping is
    required, thus hiding the remaining entries.

    An empty list has the same meaning has a list containing a single
    mone, that is, indicated that no grouping is required.

    (Some implementations uses zero instead of mone here, hence we
    support both. *)

    int_currency_symbol: string;
    (** International currency symbol. This is formed by the
    three-letter ISO-4217 entry code for the currency, like "USD" for
    U.S.Dollar or "GBP" for the Pound Sterling. *)

    int_currency_symbol_sep: string;
    (** The character used to separate the internation currency symbol
    from the monetary quantity. *)

    loc_currency_symbol : string;
    (** Local currency symbol. *)

    mon_decimal_point : string;
    (** Decimal-point separator used for monetary quantities. *)

    mon_thousands_sep : string;
    (** Separators used to delimit groups of digits to the left of the
    decimal point for monetary quantities. *)

    mon_grouping : int list;
    (** Specifies the amount of digits that form each of the groups to
    be separated by mon_thousands_sep separator for monetary
    quantities. See grouping description above. *)

    mon_positive_sign : string;
    (** Sign to be used for nonnegative (positive or zero) monetary
    quantities. *)

    mon_negative_sign : string;
    (** Sign to be used for negative monetary quantities. *)

    int_frac_digits : int;
    (** Amount of fractional digits to the right of the decimal point
    for monetary quantities in the international format. *)

    loc_frac_digits : int;
    (** Amount of fractional digits to the right of the decimal point
    for monetary quantities in the local format. *)

    p_cs_precedes: bool;
    (** Whether the currency symbol should precede nonnegative
    (positive or zero) monetary quantities. *)

    n_cs_precedes : bool;
    (** Whether the currency symbol should precede negative monetary
    quantities. *)

    p_sep_by_space : bool;
    (** Whether a space should appear between the currency symbol and
    nonnegative (positive or zero) monetary quantities. *)

    n_sep_by_space : bool;
    (** Whether a space should appear between the currency symbol and
    negative monetary quantities. *)

    p_sign_posn	: sign_posn;
    (** Position of the sign for nonnegative (positive or zero)
    monetary quantities. *)

    n_sign_posn	: sign_posn;
    (** Position of the sign for negative monetary quantities. See
    p_sign_posn above.  *)
  } and sign_posn =
    | Parentheses
    (** Currency symbol and quantity surrounded by parentheses. *)

    | Before_everything
    (** Sign before the quantity and currency symbol. *)

    | After_everything
    (** Sign after the quantity and currency symbol. *)

    | Before_currency_symbol
    (** Sign right before currency symbol. *)

    | After_currency_symbol
    (** Sign right after currency symbol. *)

  (** The standard locale used when no other information is supplied. *)
  val stdlocale : t
end


(** Dynamically typed values. *)
module Value :
sig

  (** The type tracking ['a]. *)
  type 'a kind =
  | Bool : bool kind
  | Int : int kind
  | Char : char kind
  | String : string kind
  | Float : float kind

  (** The abstract type of dynamically typed values. *)
  type t


  (** {6 Creating and examining values} *)

  val make : 'a kind -> 'a -> t
  (** [make kind value] create a dynamically typed value. *)

  val get_option : 'a kind -> t -> 'a option
  (** Get a dynamical value in a safe manner.

  It is important to note that there is no implicit conversions
  performed on the value. *)

  val get : 'a kind -> t -> 'a
  (** Get a dynamical value in an unsafe manner.

  @raise Failure if the value is of anouther kind. *)

  val is_of_kind : 'a kind -> t -> bool
  (** Predicate recognising dynamical values of a specific kind. *)

  val is_of_same_kind : t -> t -> bool
  (** Predicate recognising dynamic of values of the same kind. *)


  (** {6 Comparison} *)

  val equal : t -> t -> bool
  (** Equality test for dynamic values. *)

  val compare : t -> t -> int
  (** Comparison function for dynamic values. *)

  (** {6 Persistence} *)

  val taste : 'a kind -> string -> bool
  (** [taste k s] recognise [s] if it represents a value of kind [k]. *)

  val to_string : t -> string
  (** [to_string v] transform [v] to a string.

  This string can be written in a file and read back later with [of_string]. *)

  val of_string : string -> t
  (** [of_string s] convert a string to a dynamic value. *)

  val of_string_kind : 'a kind -> string -> 'a
  (** [of_string_kind k s] convert [s] to a dynamic value of kind [k].

  @raise Failure if [s] cannot be converted to a value of kind [k]. *)

  val of_string_same : t -> string -> t
  (** [of_string_same v s] convert [s] to a dynamic value of the same kind a [v].

  @raise Failure if [s] cannot be converted to a value of that kind. *)

  val reader_kind : 'a kind -> Scanf.Scanning.in_channel -> 'a
  (** Custom reader to use with [scanf]. *)

  val reader_same : t -> Scanf.Scanning.in_channel -> t
  (** Custom reader to use with [scanf]. *)

  val printer : Format.formatter -> t -> unit
  (** A printer for the toplevel. *)


  (** {6 Formatted output}

  The low-level functions [add], [add_foramtted] and [add_text] should
  only be used in emergency cases.  The preferred way to output values
  is through the use of messages. *)

  (** The type of localisation parameters. *)
  type locale = Locale.t

  (** The type of formatting information for values. *)
  type format = {
    specifier : char;

    left_justify : bool;
    (** Left-justify within the given field width; Right justification
    is the default (see width sub-specifier). *)

    always_signed : bool;
    (** Forces to precede the result with a plus or minus sign (+ or -)
    even for positive numbers. By default, only negative numbers
    are preceded with a - sign. *)

    align_sign : bool;
    (** If no sign is going to be written, a blank space is inserted
    before the value.

    This implies that the [align_sign] flag is ignored when the the
    [always_signed] flag is set, as clarified by ANSI.

    {v
    ``If the space and + flags both appear, the space
      flag will be ignored.''
      -- ANSI X3J11
    v}*)

    alternative_format : bool;
    (** Choose an alternative format, depending on the value. *)

    pad_with_zeroes : bool;
    width: int;
    precision: int;
  }

  (** The standard format used when no other format is supplied. *)
  val stdformat : format

  (** Convert format information to concrete form. *)
  val format_to_string : format -> string

  (** Convert concrete form to format information.

  @raise Invalid_arg when the given argument cannot be converted. *)
  val format_of_string : string -> format

  (** The type of data buffers. *)
  type buffer = Buffer.t

  (** Write a formatted value to a data buffer.

  The first [buffer] given in argument is a scratch buffer that is
  empty when the function is called. The function is expected to add
  the appropriate words to the second buffer.  *)
  val add_formatted : buffer -> locale -> buffer -> format -> t -> unit

  (** Write a value to a data buffer.

  See [add_formatted] for comments on arguments. *)
  val add : buffer -> locale -> buffer -> t -> unit

  (** Write a text to a data buffer.

  The precise interpretation of text has to be specified by concrete
  implementations.

  See [add_formatted] for comments on arguments. *)
  val add_text : buffer -> locale -> buffer -> string -> unit
end


module Classification :
sig

  (** We imitate the classification of the syslog facility.  The normal
  classification for a message is [Info].  The highest [Emergency],
  [Alert] or [Critical] error levels are most probably only relevant
  for critical systems.  The highest level a typical command line
  utility should use is [Error], usually indicating an unrecoverable
  error. *)
  type t =
  | Debug
  | Info
  | Notice
  | Warning
  | Error
  | Critical
  | Alert	(** A condition that should be corrected immediately. *)
  | Emergency	(** A panic condition. *)

  (** The type of localisations parameters. *)
  type locale = Value.locale

  (** The type of data buffers. *)
  type buffer = Buffer.t

  (** Prologue to message adding. *)
  val add_pre : locale -> buffer -> t -> string -> unit

  (** Epilogue to message adding. *)
  val add_post : locale -> buffer -> t -> string -> unit

  (** The type of output channels. *)
  type out_channel = Pervasives.out_channel

  (** Message classifications are ordered. *)
  val compare : t -> t -> int

  (** Prologue and epilogue to sending messages, the string is the id
  of the message being sent. *)
  val send_pre : locale -> out_channel -> t -> string -> unit
  val send_post : locale -> out_channel -> t -> string -> unit

  val of_string : string -> t
  (** Convert a string to a classification. The string must be in
  lowercase.

  @raise Invalid_argument if the string cannot be converted. *)

  val to_string : t -> string
  (** Convert a classification to string. *)
end

(** Diagnostic messages. *)
module Message :
sig

  (** The type of message classification. *)
  type classification =
    Classification.t

  (** The abstract type of message sinks. *)
  type sink

  (** The type of core values used by the application. *)
  type value =
    Value.t

  (** [send sink clas id binding] require the sink [sink] to send the
  message identified by [id] with classification [c], substituting
  message variables according to the [binding]. *)
  val send : sink -> classification ->
    string -> (string * value) list -> unit
end


(** Application components. *)
module Component :
sig

  (** The concrete type of application components. *)
  type info = {
    name : string;
    version : string;
    description : string;
    require: string list;
    provide : string list;
    config_prefix : string list;
    getopt_prefix : char option;
  }

  (** The type of callbacks. *)
  type callback = info -> unit

  val sink : info -> Message.sink
  (** Get the message sink dedicated to a given component. *)

  (** Register a component. *)
  val register : ?bootstrap:callback -> ?shutdown:callback -> info -> unit
end


(** Configuration values. *)
module Configuration :
sig

  (** The type tracking ['a]. *)
  type 'a kind =
    'a Value.kind

  (** The type of configuration items tracking a value of type ['a]. *)
  type 'a t

  (** The type of application components. *)
  type component =
    Component.info

  val make : 'a kind -> component ->
    ?flag:char -> ?env:string -> ?shy:bool ->
    string -> 'a -> string -> 'a t
  (** [make kind comp name default description] create a configuration
      item holding a value of type ['a].  This configuration value can
      be manipulated through the functions [set] and [get] below.

      @param flag the letter used for command line flag.
      @param env the environment variable used to get a value.
      @param shy flag governing description in the short help. *)

  val get : 'a t -> 'a
  (** [get item] return the value of the given configuration [item]. *)

  val set : 'a t -> 'a -> unit
  (** [set item value] set the [value] of the given configuration [item]. *)

  (** The type of configuration specifications. *)
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


(** Analyse of command line arguments. *)
module Getopt :
sig

  type t
  (** The abstract type of getopt thingies. *)

  val flag : char -> (unit -> unit) -> string -> t
  (** [flag c callback description] return a getopt thingie recognising
      the flag [c] on the command line and triggering the given
      [callback]. *)

  val make : 'a Value.kind -> char -> ('a -> unit) -> string -> t
  (** [make kind c callback description] return a getopt thingie recognising
      the flag [c] on the command line. When this flag occurs on the command
      line, the [callback] is called on the option argument. The message
      [description] may be displayed in the help screen of the program.

      If the flag is present without an argument or if the argument cannot be
      converted, then the guest program will terminate with exit code
      [EXIT_USAGE] (64), displaying appropriate message and a usage notice.

      The callback may raise [Invalid_argument] to indicate that a specific
      value of the option is not supported.  The guest program will terminate
      with exit code [EXIT_USAGE] (64), displaying appropriate message and
      a usage notice. *)

  type note
  (** The abstract type of usage notes. *)

  val note : string -> string -> note
  (** [note title text] create a note to be displayed in the help summary
      of the application.  This can be used to add copyright information
      or any other kind of credit or information to the help summary. *)
end

val run : string -> string -> string ->
  ?options:(Getopt.t list) -> ?notes:(Getopt.note list) ->
  ?configuration:Configuration.spec ->
  (string list -> unit) -> unit
(** [run name usage description ?options ?notes main] run the application.

The function [main] is passed the list of remaining arguments on the
command line. *)

val help : unit -> unit
(** Output the help summary associated to the application and
terminates the application with exit code [SysExits.EXIT_SUCCESS].

This is a magic function which can be called from code executed in the
[run] function, if this condition is not fullfilled, it raises a
[Invalid_argument].

@raise Invalid_argument if not called under [run]. *)

val usage : string  -> unit
(** Output a message and the usage summary associated to the application,
then terminate the application with exit code [SysExits.EXIT_USAGE].

This is a magic function which can be called from code executed in the
[run] function, if this condition is not fullfilled, it raises a
[Invalid_argument].

@raise Invalid_argument if not called under [run]. *)
