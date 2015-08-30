(* Wordcount -- Counting words

Author: Michael Grünewald
Date: Mon Jan 20 23:12:00 CET 2014

Gasoline (https://github.com/michipili/gasoline)
This file is part of Gasoline

Copyright © 2014 Michael Grünewald

This file must be used under the terms of the CeCILL-B.
This source file is licensed as described in the file COPYING, which
you should have received as part of this distribution. The terms
are also available at
http://www.cecill.info/licences/Licence_CeCILL-B_V1-en.txt *)
module Application =
  Gasoline_Plain_Application

module Component_count :
sig
  val comp : Application.Component.t

  val count_channel : in_channel -> Count.t
  val count_file : string -> Count.t

end


module Component_display :
sig
  val comp : Application.Component.t

  module Configuration :
  sig
    val bytes : unit -> bool
    val lines : unit -> bool
    val chars : unit -> bool
    val words : unit -> bool
    val longest : unit -> bool
  end
  val print_count : string -> Count.t -> unit
  val print_summary : Count.t list -> unit
end


val count_channel : in_channel -> unit
val count_file : string -> unit
val count_files : string list -> unit
val main : string list -> unit
