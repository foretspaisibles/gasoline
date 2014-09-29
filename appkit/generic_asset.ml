(* Generic_asset -- Generic assets

Author: Michael Grünewald
Date: Wed Sep  3 12:55:04 CEST 2014

Gasoline (https://github.com/michipili/gasoline)
This file is part of Gasoline

Copyright © 2014 Michael Grünewald

This file must be used under the terms of the CeCILL-B.
This source file is licensed as described in the file COPYING, which
you should have received as part of this distribution. The terms
are also available at
http://www.cecill.info/licences/Licence_CeCILL-B_V1-en.txt *)
open Printf

let _package = ref ""
let _version = ref ""
let _prefix = ref ""
let _bindir = ref ""
let _sbindir = ref ""
let _libexecdir = ref ""
let _datadir = ref ""
let _sysconfdir = ref ""
let _usrconfdir = ref ""
let _sharedstatedir = ref ""
let _localstatedir = ref ""

let meta_default list arg =
  match arg with
  | Some p -> p
  | None -> List.fold_left Filename.concat !_prefix list

(* Default to an absolute path. *)
let abs_default path arg =
  meta_default [ path ] arg

(* Default to a site-wide path. *)
let site_default path arg =
  meta_default [ path ] arg

(* Default to a package/version specific path. *)
let package_default path arg =
  meta_default [ path; !_package; !_version ] arg

let setup f ~package ~version ?(prefix = "/usr/local")
    ?bindir ?sbindir ?libexecdir ?datadir ?sysconfdir ?usrconfdir
    ?sharedstatedir ?localstatedir =
  let prepare (r,f,x) =
    r := f x
  in
  let home =
    try Sys.getenv "HOME"
    with Not_found -> "."
  in
  begin
    _package := package;
    _version := version;
    _prefix := prefix;
    List.iter prepare [
      _bindir,		site_default "bin",		bindir;
      _sbindir,		site_default "sbin",		sbindir;
      _libexecdir,	package_default "libexec",	libexecdir;
      _datadir,		package_default "share",	datadir;
      _sysconfdir,	package_default "etc",		sysconfdir;
      _usrconfdir,	abs_default home,		usrconfdir;
      _sharedstatedir,	package_default "com",		sharedstatedir;
      _localstatedir,	package_default "var",		localstatedir;
    ];
    f
  end

let bin =
  Filename.concat !_bindir

let sbin =
  Filename.concat !_sbindir

let libexec =
  Filename.concat !_libexecdir

let data =
  Filename.concat !_datadir

let sysconf =
  Filename.concat !_sysconfdir

let usrconf =
  Filename.concat !_usrconfdir

let sharedstate =
  Filename.concat !_sharedstatedir

let localstate =
  Filename.concat !_localstatedir

let usrconffile () =
  usrconf (sprintf ".%s" !_package)

let sysconffile name =
  sysconf (sprintf "%s.conf" name)
