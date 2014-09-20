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
open Printf

module Prototype =
struct
  module Locale =
    CType.Locale

  module Value =
    CType.Value

  module Classification =
    CType.Classification

  module Database =
    CType.Database

  module Buffer =
    CType.Buffer

  module Data =
    CType.Data
end

module Locale =
  Prototype.Locale

module Value =
  Prototype.Value

module Classification =
  Prototype.Classification

module CSink =
  Generic_message.Sink.Make(Prototype)

module CSinkInitializer =
struct
  type sink = CSink.t
  type connection_token = Prototype.Database.connection_token
  type locale = Prototype.Locale.t
  type out_channel = Prototype.Data.out_channel
  let connection_token _ = ()
  let locale _ = Prototype.Locale.stdlocale
  let out_channel_lst _ = [ Pervasives.stderr ]
end

module InternalApplication =
  Generic_application.Make(CSink)(CSinkInitializer)(Prototype)

module Component =
  InternalApplication.Component

module Configuration =
  InternalApplication.Configuration

module Message =
struct
  type classification =
    Classification.t

  type sink =
    CSink.t

  type value =
    Value.t

  let send =
    CSink.send
end

module Getopt =
struct

  type t =
    Getopt.t

  type note =
    Getopt.note

  type spec =
    Getopt.spec

  let flag c callback description =
    Getopt.flag c callback description

  let make kind c callback description =
    Getopt.concrete (Value.of_string_kind kind) c callback description

  let note title text =
    Getopt.note title text
end


let run name usage description ?options ?notes ?configuration main =
  InternalApplication.run
    name usage description ?options ?notes ?configuration main

let help () =
  InternalApplication.help ()

let usage () =
  InternalApplication.usage()
