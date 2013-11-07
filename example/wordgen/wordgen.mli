(* Wordgen -- Word generator

Author: Michael Grünewald
Date: Fri Nov  8 07:55:44 2013 CET

Gasoline (https://github.com/michipili/gasoline)
This file is part of Gasoline

Copyright © 2013 Michael Grünewald

This file must be used under the terms of the CeCILL-B.
This source file is licensed as described in the file COPYING, which
you should have received as part of this distribution. The terms
are also available at
http://www.cecill.info/licences/Licence_CeCILL-B_V1-en.txt *)
open CApplication

module Component_automaton :
sig
  val comp : Component.t

  module Configuration :
  sig
    val generate_n : int Configuration.t
    val generate_min : int Configuration.t
    val generate_max : int Configuration.t
    val length : int Configuration.t
  end

  val compile : string -> Persistant.t
  val generate : Persistant.t -> string list
end

module Component_library :
sig
  val comp : Component.t

  module Configuration :
  sig
    val dump : string Configuration.t
  end

  val load : string -> Persistant.t
  val save : string -> Persistant.t -> unit
  val list : unit -> string list
end


val help : unit -> unit
val list : unit -> unit
val compile : string -> unit
val generate : string -> unit
val main : unit -> unit
