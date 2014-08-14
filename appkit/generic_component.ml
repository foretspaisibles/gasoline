(* Generic_component -- Application components

Author: Michael Grünewald
Date: Wed Sep 17 10:16:30 CEST 2014

Gasoline (https://github.com/michipili/gasoline)
This file is part of Gasoline

Copyright © 2014 Michael Grünewald

This file must be used under the terms of the CeCILL-B.
This source file is licensed as described in the file COPYING, which
you should have received as part of this distribution. The terms
are also available at
http://www.cecill.info/licences/Licence_CeCILL-B_V1-en.txt *)

type info = {
  name : string;
  version : string;
  description : string;
  require: string list;
  provide : string list;
  config_prefix : string list;
  getopt_prefix : char option;
}

type callback = info -> unit

type t = {
  info: info;
  bootstrap: callback;
  shutdown: callback;
}

let make info bootstrap shutdown = {
  info;
  bootstrap;
  shutdown;
}

let bootstrap lst =
  failwith "bootstrap: not implemented"

let shutdown lst =
  failwith "shutdown: not implemented"
