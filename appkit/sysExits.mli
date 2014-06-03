(* SysExits -- System exit codes

Author: Michael Grünewald
Date: Thu Apr 24 12:05:03 CEST 2014

Gasoline (https://github.com/michipili/gasoline)
This file is part of Gasoline

Copyright © 2014 Michael Grünewald

This file must be used under the terms of the CeCILL-B.
This source file is licensed as described in the file COPYING, which
you should have received as part of this distribution. The terms
are also available at
http://www.cecill.info/licences/Licence_CeCILL-B_V1-en.txt *)
(** System exit codes

The {i sysexits(3)} manual page gives advice for the use of system
exit codes of a program. The definitions in this module helps to adhere to
these recommendations. *)

(** {i From sysexits(3):} According to {i style(9)}, it is not a good
practice to call {i exit(3)} with arbitrary values to indicate a failure
condition when ending a program.  Instead, the pre-defined exit codes
from {i sysexits(3)} should be used, so the caller of the process can get a
rough estimation about the failure class without looking up the source
code.

{b Remark:} We added sybolic values [EXIT_SUCCESS] and [EXIT_FAILURE]
to the list provided by {i sysexits(3)}. *)

(** The type of symbolic error codes. The numerical values
corresponding to the symbolical ones are given in parenthesis for
easy reference. *)
type t =

  | EXIT_SUCCESS
  (** [EXIT_SUCCESS] (0) The program succeeded. *)

  | EXIT_FAILURE
  (** [EXIT_FAILURE] (1) The program failed for an unspecified
  reason. *)

  | EXIT_USAGE
  (** [EXIT_USAGE] (64) The command was used incorrectly, e.g., with
  the wrong number of arguments, a bad flag, a bad syntax in a
  parameter, or whatever. *)

  | EXIT_DATAERR
  (** [EXIT_DATAERR] (65) The input data was incorrect in some
  way. This should only be used for user's data and not system
  files. *)

  | EXIT_NOINPUT
  (** [EXIT_NOINPUT] (66) An input file (not a system file) did not
  exist or was not readable. This could also include errors like ``No
  message'' to a mailer (if it cared to catch it). *)

  | EXIT_NOUSER
  (** [EXIT_NOUSER] (67) The user specified did not exist. This might
  be used for mail addresses or remote logins. *)

  | EXIT_NOHOST
  (** [EXIT_NOHOST] (68) The host specified did not exist. This is
  used in mail addresses or network requests. *)

  | EXIT_UNAVAILABLE
  (** [EXIT_UNAVAILABLE] (69) A service is unavailable. This can occur
  if a support program or file does not exist. This can also be used
  as a catchall message when something you wanted to do does not work,
  but you do not know why. *)

  | EXIT_SOFTWARE
  (** [EXIT_SOFTWARE] (70) An internal software error has been
  detected. This should be limited to non-operating system related
  errors as possible. *)

  | EXIT_OSERR
  (** [EXIT_OSERR] (71) An operating system error has been
  detected. This is intended to be used for such things as ``cannot
  fork'', ``cannot create pipe'', or the like. It includes things like
  getuid returning a user that does not exist in the passwd file. *)

  | EXIT_OSFILE
  (** [EXIT_OSFILE] (72) Some system file (e.g., /etc/passwd,
  /var/run/utmp, etc.) does not exist, cannot be opened, or has some
  sort of error (e.g., syntax error). *)

  | EXIT_CANTCREAT
  (** [EXIT_CANTCREAT] (73) A (user specified) output file cannot be
  created. *)

  | EXIT_IOERR
  (** [EXIT_IOERR] (74) An error occurred while doing I/O on some
  file. *)

  | EXIT_TEMPFAIL
  (** [EXIT_TEMPFAIL] (75) Temporary failure, indicating something
  that is not really an error. In sendmail, this means that a mailer
  (e.g.)  could not create a connection, and the request should be
  reattempted later. *)

  | EXIT_PROTOCOL
  (** [EXIT_PROTOCOL] (76) The remote system returned something that
  was ``not possible'' during a protocol exchange. *)

  | EXIT_NOPERM
  (** [EXIT_NOPERM] (77) You did not have sufficient permission to
  perform the operation. This is not intended for file system
  problems, which should use EX_NOINPUT or EX_CANTCREAT, but rather
  for higher level permissions. *)

  | EXIT_CONFIG
  (** [EXIT_CONFIG] (78) Something was found in an unconfigured or
  misconfigured state. *)

  | EXIT_RANDOM of int
  (** For emitting or decoding other exit codes. *)

val exit : t -> 'a
(** Terminate the process as [Pervasives.exit] but uses a symbolic
exit code. *)

val to_int : t -> int
(** [to_int x] convert a symbolic error code to an error code, suited
to be used with [Pervasives.exit]. *)

val of_int : int -> t
(** [of_int x] convert the given exit code to a symbolic exit
code. The [EXIT_RANDOM] constructor is used for codes which do not
have a name. *)
