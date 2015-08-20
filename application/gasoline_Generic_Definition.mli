(* Gasoline_Generic_Type -- Definition of generic types

Author: Michael Grünewald
Date: Wed May 22 07:04:55 CEST 2013

Copyright © 2013 Michael Grünewald

This file must be used under the terms of the CeCILL-B.
This source file is licensed as described in the file COPYING, which
you should have received as part of this distribution. The terms
are also available at
http://www.cecill.info/licences/Licence_CeCILL-B_V1-en.txt *)
(** Definition of generic types. These types abstract essential data
structures used by the [Generic_message], [Generic_diagnostic] and
[Generic_application] facilities. *)

(** Words and blocks based I/O. *)
module type DATA =
sig

  (** The type of data words. *)
  type word

  (** The type of data blocks. *)
  type block

  (** The type of data output channels. *)
  type out_channel

  (** Write a word on the given output channel. *)
  val output_word : out_channel -> word -> unit

  (** [output oc buf pos len] write [len] words from block buf,
  starting at offset [pos], to the given output channel [oc].

  @raise Invalid_argument "output" if [pos] and [len] do not designate a
  valid substring of [buf]. *)
  val output_block : out_channel -> block -> int -> int -> unit

  (** Flush an output channel, to empty its internal buffer. *)
  val flush : out_channel -> unit

end


(** Extensible data buffers.

This signature abstracts a subset of the [Buffer] module found in the
standard library. *)
module type BUFFER =
sig

  (** The type of data words. *)
  type word

  (** The type of data blocks. *)
  type block

  (** The type of data output channels. *)
  type out_channel

  type t
  (** The abstract type of buffers. *)

  val create : int -> t
  (** [create n] create a fresh buffer, initially empty.  The [n]
  parameter is the initial size in words of the internal storage
  that holds the buffer contents. *)

  val contents : t -> block
  (** Return a copy of the current contents of the buffer.
  The buffer itself is unchanged. *)

  val nth : t -> int -> word
  (** Get the n-th word of the buffer. Raise [Invalid_argument] if
  this index is out of bounds *)

  val length : t -> int
  (** Return the number of words currently held by the buffer. *)

  val clear : t -> unit
  (** Empty the buffer. *)

  val reset : t -> unit
  (** Empty the buffer and deallocate the internal storage holding the
  buffer contents. *)

  val add_word : t -> word -> unit
  (** [add_word b w] append the word [w] at the end of the buffer [b]. *)

  val add_block : t -> block -> unit
  (** [add_block b s] append the block [s] at the end of the buffer [b]. *)

  val add_buffer : t -> t -> unit
  (** [add_buffer b1 b2] append the current contents of buffer [b2]
      at the end of buffer [b1].  Buffer [b2] is not modified. *)

  val output_buffer : out_channel -> t -> unit
  (** [output_buffer oc b] write the current contents of buffer [b]
      on the output channel [oc]. Buffer [b] is not modified. *)
end


(** Description of application values.

The preparation, or formatting, of these values is governed by two
sets of parameters.  The first set of parameters is referred to
collectively as a [locale]. It can be used to actually emulate locale
settings as they are provided by other systems.  The second set of
parameters is referred to as a [format], it is much like formatting
information used by the [printf] facility.

The two sets of parameters [locale] and [format] are distinct and
orthogonal: the first one is usually bound to an output session while
the second is set for each value. *)
module type VALUE =
sig

  (** The type of values that can be conveyed by application messages
      and application configuration parameters. *)
  type t

  (** The type of localisation parameters. *)
  type locale

  (** The type of formatting informations for values. *)
  type format

  (** The standard format. *)
  val stdformat : format

  (** Convert format information to a concrete form. *)
  val format_to_string : format -> string

  (** Convert concrete form to format information.

  It returns [stdformat] when called on the empty string.  The details
  of the format syntax is implementation-dependant.

  @raise Invalid_arg when the given argument cannot be converted. *)
  val format_of_string : string -> format

  (** The type of data buffers. *)
  type buffer

  (** Write a formatted value to a data buffer.

  The first [buffer] given in argument is a scratch buffer that is
  empty when the function is called and can be used as a scratch area.
  The function is expected to add the appropriate words to the second
  buffer. *)
  val add_formatted : buffer -> locale -> buffer -> format -> t -> unit

  (** Write a value to a data buffer.

  See [add_formatted] for the description of arguments. *)
  val add : buffer -> locale -> buffer -> t -> unit

  (** Write a text to a data buffer.

  The actual interpretation of text has to be specified by concrete
  implementations.

  See [add_formatted] for the description of other arguments. *)
  val add_text : buffer -> locale -> buffer -> string -> unit

end


(** Reading the message database. *)
module type MESSAGEDB =
sig

  (** The abstract type of message database handles. *)
  type t

  (** The abstract type of connection tokens. *)
  type connection_token

  (** Open a descriptor on a message database.

  @raise Failure when connection fails.*)
  val opendb : connection_token -> t

  val find : t -> string -> string
  (** [find db key] return a concrete form for the lexemes associated
  with [id] in the database opened for descriptor [db].

  @raise Not_found if the [key] has no associated data. *)

  (** Close the given descriptor.

  Using a closed descriptor is an error and raises a [Failure]. *)
  val close : t -> unit

end


(** Classifying messages.

A well known classification is the one used by the UNIX {i syslog} facility.
Parametrisation
of this module allows to define new classification schemes, however,
it is probably a good idea to use only classification consisting of
a finite, fixed, hierarchised set, of tags.  *)
module type PRIORITY =
sig

  (** The type of message priorities. *)
  type t

  (** Message priorities are totally ordered. *)
  val compare : t -> t -> int

  (** The type of localisations parameters. *)
  type locale

  (** The type of data buffers. *)
  type buffer

  (** Prologue to message adding. *)
  val add_pre : locale -> buffer -> t -> string -> unit

  (** Epilogue to message adding. *)
  val add_post : locale -> buffer -> t -> string -> unit

  (** The type of output channels. *)
  type out_channel

  (** Prologue to message sending. *)
  val send_pre : locale -> out_channel -> t -> string -> unit

  (** Epilogue to message sending. *)
  val send_post : locale -> out_channel -> t -> string -> unit

end


(** Localising preparation of messages.

When preparing a value for its printing in a message, some decisions
are tied to to the localisation parameters defined by the program
operator, some others to the value itself.  These are [locale] and
[format]. *)
module type LOCALE =
sig

  (** The type of localisation parameters. *)
  type t

  (** The standard localisation parameters, analogous to the "C"
  locale for C-stylish programs. *)
  val stdlocale : t

end
