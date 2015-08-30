(* Gasoline_SysExits -- System exit codes

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
type t =
  | EXIT_SUCCESS
  | EXIT_FAILURE
  | EXIT_USAGE
  | EXIT_DATAERR
  | EXIT_NOINPUT
  | EXIT_NOUSER
  | EXIT_NOHOST
  | EXIT_UNAVAILABLE
  | EXIT_SOFTWARE
  | EXIT_OSERR
  | EXIT_OSFILE
  | EXIT_CANTCREAT
  | EXIT_IOERR
  | EXIT_TEMPFAIL
  | EXIT_PROTOCOL
  | EXIT_NOPERM
  | EXIT_CONFIG
  | EXIT_RANDOM of int

let to_int x =
  match x with
    | EXIT_SUCCESS -> 0
    | EXIT_FAILURE -> 1
    | EXIT_USAGE -> 64
    | EXIT_DATAERR -> 65
    | EXIT_NOINPUT -> 66
    | EXIT_NOUSER -> 67
    | EXIT_NOHOST -> 68
    | EXIT_UNAVAILABLE -> 69
    | EXIT_SOFTWARE -> 70
    | EXIT_OSERR -> 71
    | EXIT_OSFILE -> 72
    | EXIT_CANTCREAT -> 73
    | EXIT_IOERR -> 74
    | EXIT_TEMPFAIL -> 75
    | EXIT_PROTOCOL -> 76
    | EXIT_NOPERM -> 77
    | EXIT_CONFIG -> 78
    | EXIT_RANDOM r -> r

let of_int x =
  match x with
    | 0 -> EXIT_SUCCESS
    | 255 -> EXIT_FAILURE
    | 64 -> EXIT_USAGE
    | 65 -> EXIT_DATAERR
    | 66 -> EXIT_NOINPUT
    | 67 -> EXIT_NOUSER
    | 68 -> EXIT_NOHOST
    | 69 -> EXIT_UNAVAILABLE
    | 70 -> EXIT_SOFTWARE
    | 71 -> EXIT_OSERR
    | 72 -> EXIT_OSFILE
    | 73 -> EXIT_CANTCREAT
    | 74 -> EXIT_IOERR
    | 75 -> EXIT_TEMPFAIL
    | 76 -> EXIT_PROTOCOL
    | 77 -> EXIT_NOPERM
    | 78 -> EXIT_CONFIG
    | r -> EXIT_RANDOM r

let exit x = Pervasives.exit (to_int x)
