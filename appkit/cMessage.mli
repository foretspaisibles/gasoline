(* cMessage -- Messages for C-Stylish applications

Author: Michael Grünewald
Date: Wed Sep  5 07:35:09 CEST 2012

Copyright © 2012 Michael Grünewald

This file must be used under the terms of the CeCILL-B.
This source file is licensed as described in the file COPYING, which
you should have received as part of this distribution. The terms
are also available at
http://www.cecill.info/licences/Licence_CeCILL-B_V1-en.txt *)
(** Messages for C-Stylish applications. *)

open CType

(** Concrete C-like message facility. *)
module Scribe : Generic_message.Scribe.S
  with type classification = Classification.t
   and type locale = Locale.t
   and type connection_token = Database.connection_token
   and type out_channel = Data.out_channel
   and type value = Value.t
   and type buffer = Buffer.t
   and type block = Data.block

module Sender : Generic_message.Sender.S
  with type classification = Classification.t
   and type locale = Locale.t
   and type connection_token = unit
   and type out_channel = Pervasives.out_channel
   and type value = Value.t

module Sink : Generic_message.Sink.S
  with type classification = Classification.t
   and type locale = Locale.t
   and type connection_token = Database.connection_token
   and type out_channel = Data.out_channel
   and type value = Value.t
