(* cMessage -- Messages for C-Stylish applications

Author: Michael Grünewald
Date: Wed Sep  5 07:35:09 CEST 2012

Copyright © 2012 Michael Grünewald

This file must be used under the terms of the CeCILL-B.
This source file is licensed as described in the file COPYING, which
you should have received as part of this distribution. The terms
are also available at
http://www.cecill.info/licences/Licence_CeCILL-B_V1-en.txt *)
open CType

module P =
struct
  module Data = Data
  module Locale = Locale
  module Buffer = Buffer
  module Value = Value
  module Database = Database
  module Classification = Classification
end

module Scribe = Generic_message.Scribe.Make(P)
module Sender = Generic_message.Sender.Make(P)
module Sink =  Generic_message.Sink.Make(P)
