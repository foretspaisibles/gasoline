(* Gasoline_Plain_Message -- Messages for C-Stylish applications

Author: Michael Grünewald
Date: Wed Sep  5 07:35:09 CEST 2012

Copyright © 2012 Michael Grünewald

This file must be used under the terms of the CeCILL-B.
This source file is licensed as described in the file COPYING, which
you should have received as part of this distribution. The terms
are also available at
http://www.cecill.info/licences/Licence_CeCILL-B_V1-en.txt *)
module P =
struct
  module Data = Gasoline_Plain_Definition.Data
  module Locale = Gasoline_Plain_Definition.Locale
  module Buffer = Gasoline_Plain_Definition.Buffer
  module Value = Gasoline_Plain_Definition.Value
  module MessageDB = Gasoline_Plain_Definition.MessageDB
  module Priority = Gasoline_Plain_Definition.Priority
end

module Scribe = Gasoline_Generic_Message.Scribe.Make(P)
module Sender = Gasoline_Generic_Message.Sender.Make(P)
module Sink =  Gasoline_Generic_Message.Sink.Make(P)
