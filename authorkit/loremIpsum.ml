(* LoremIpsum -- Text generator

Author: Michael Grünewald
Date: Mon  9 Mar 2009 18:47:07 CET

Gasoline (https://github.com/michipili/gasoline)
This file is part of Gasoline

Copyright © 2008–2014 Michael Grünewald

This file must be used under the terms of the CeCILL-B.
This source file is licensed as described in the file COPYING, which
you should have received as part of this distribution. The terms
are also available at
http://www.cecill.info/licences/Licence_CeCILL-B_V1-en.txt *)
let title_max = 52	(* The maximum length of a title *)
let para_min = 300	(* The minimum length of a paragraph *)
let para_max = 1200	(* The maximum length of a paragraph *)
let text_min = 5	(* The minimal number of paragraphs in a text *)
let text_max = 13	(* The maximal number of paragraphs in a text *)

let choose a =
  a.(Random.int (Array.length a))

let text_sample = [|
  "Lorem ipsum dolor sit amet, consectetuer adipiscing elit.";
  "Duis sem velit, ultrices et, fermentum auctor, rhoncus ut, ligula.";
  "Phasellus at purus sed purus cursus iaculis.";
  "Suspendisse fermentum.";
  "Pellentesque et arcu.";
  "Maecenas viverra.";
  "In consectetuer, lorem eu lobortis egestas, velit odio imperdiet eros, \
   sit amet sagittis nunc mi ac neque.";
  "Sed non ipsum.";
  "Nullam venenatis gravida orci.";
  "Curabitur nunc ante, ullamcorper vel, auctor a, aliquam at, tortor.";
  "Etiam sodales orci nec ligula.";
  "Sed at turpis vitae velit euismod aliquet.";
  "Fusce venenatis ligula in pede.";
  "Pellentesque viverra dolor non nunc.";
  "Donec interdum vestibulum libero.";
  "Morbi volutpat.";
  "Phasellus hendrerit.";
  "Quisque dictum quam vel neque.";
  "Quisque aliquam, nulla ac scelerisque convallis, nisi ligula sagittis \
   risus, at nonummy arcu urna pulvinar nibh.";
  "Nam pharetra.";
  "Nam rhoncus, lectus vel hendrerit congue, nisl lorem feugiat ante, \
   in fermentum erat nulla tristique arcu.";
  "Mauris et dolor.";
  "Vestibulum ante ipsum primis in faucibus orci luctus et \
   ultrices posuere cubilia Curae; Donec gravida, ante vel ornare lacinia, \
   orci enim porta est, eget sollicitudin lectus lectus eget lacus.";
  "Praesent a lacus vitae turpis consequat semper.";
  "In commodo, dolor quis fermentum ullamcorper, urna massa volutpat massa, \
   vitae mattis purus arcu nec nulla.";
  "In hac habitasse platea dictumst.";
  "Praesent scelerisque.";
  "Nullam sapien mauris, venenatis at, fermentum at, tempus eu, urna.";
  "Vestibulum non arcu a ante feugiat vestibulum.";
  "Nam laoreet dui sed magna.";
  "Proin diam augue, semper vitae, varius et, viverra id, felis.";
  "Pellentesque sit amet dui vel justo gravida auctor.";
  "Aenean scelerisque metus eget sem.";
  "Maecenas rhoncus rhoncus ipsum.";
  "Donec nonummy lacinia leo.";
  "Aenean turpis ipsum, rhoncus vitae, posuere vitae, euismod sed, ligula.";
  "Pellentesque habitant morbi tristique senectus et netus et malesuada \
   fames ac turpis egestas.";
  "Mauris tempus diam.";
  "Maecenas justo.";
  "Sed a lorem ut est tincidunt consectetuer.";
  "Ut eu metus id lectus vestibulum ultrices.";
  "Suspendisse lectus.";
  "Vivamus posuere, ante eu tempor dictum, felis nibh facilisis sem, \
   eu auctor metus nulla non lorem.";
  "Suspendisse potenti.";
  "Integer fringilla.";
  "Morbi urna.";
  "Morbi pulvinar nulla sit amet nisl.";
  "Mauris urna sem, suscipit vitae, dignissim id, ultrices sed, nunc.";
  "Morbi a mauris.";
  "Pellentesque suscipit accumsan massa.";
  "Quisque arcu ante, cursus in, ornare quis, viverra ut, justo.";
  "Quisque facilisis, urna sit amet pulvinar mollis, \
   purus arcu adipiscing velit, non condimentum diam purus eu massa.";
  "Suspendisse potenti.";
  "Phasellus nisi metus, tempus sit amet, ultrices ac, porta nec, felis.";
  "Aliquam metus.";
  "Nam a nunc.";
  "Vivamus feugiat.";
  "Nunc metus.";
  "Vestibulum ante ipsum primis in faucibus orci luctus \
   et ultrices posuere cubilia Curae; Vivamus eu orci.";
  "Sed elementum, felis quis porttitor sollicitudin, \
   augue nulla sodales sapien, sit amet posuere quam purus at lacus.";
  "Curabitur tincidunt tellus nec purus.";
  "Nam consectetuer mollis dolor.";
  "Sed quis elit.";
  "Aenean luctus vulputate turpis.";
  "Proin lectus orci, venenatis pharetra, egestas id, tincidunt vel, eros.";
  "Nulla facilisi.";
  "Aliquam vel nibh.";
  "Vivamus nisi elit, nonummy id, facilisis non, blandit ac, dolor.";
  "Etiam cursus purus interdum libero.";
  "Nam id neque.";
  "Etiam pede nunc, vestibulum vel, rutrum et, tincidunt eu, enim.";
  "Aenean id purus.";
  "Aenean ultrices turpis.";
  "Mauris et pede.";
  "Suspendisse potenti.";
  "Aliquam velit dui, commodo quis, porttitor eget, convallis et, nisi.";
  "Maecenas convallis dui.";
  "In leo ante, venenatis eu, volutpat ut, imperdiet auctor, enim.";
  "Mauris ac massa vestibulum nisl facilisis viverra.";
  "Phasellus magna sem, vulputate eget, ornare sed, \
   dignissim sit amet, pede.";
  "Aenean justo ipsum, luctus ut, volutpat laoreet, vehicula in, libero.";
  "Praesent semper, neque vel condimentum hendrerit, \
   lectus elit pretium ligula, nec consequat nisl velit at dui.";
  "Proin dolor sapien, adipiscing id, sagittis eu, molestie viverra, mauris.";
  "Aenean ligula.";
  "Vestibulum ante ipsum primis in faucibus orci luctus et \
   ultrices posuere cubilia Curae; Suspendisse potenti.";
  "Etiam pharetra lacus sed velit imperdiet bibendum.";
  "Nunc in turpis ac lacus eleifend sagittis.";
  "Nam massa turpis, nonummy et, consectetuer id, placerat ac, ante.";
  "In tempus urna.";
  "Quisque vehicula porttitor odio.";
  "Aliquam sed erat.";
  "Vestibulum viverra varius enim.";
  "Donec ut purus.";
  "Pellentesque convallis dolor vel libero.";
  "Integer tempus malesuada pede.";
  "Integer porta.";
  "Donec diam eros, tristique sit amet, pretium vel, pellentesque ut, neque.";
  "Nulla blandit justo a metus.";
  "Curabitur accumsan felis in erat.";
  "Curabitur lorem risus, sagittis vitae, accumsan a, iaculis id, metus.";
  "Nulla sagittis condimentum ligula.";
  "Aliquam imperdiet lobortis metus.";
  "Suspendisse molestie sem.";
  "Ut venenatis.";
  "Pellentesque condimentum felis a sem.";
  "Fusce nonummy commodo dui.";
  "Nullam libero nunc, tristique eget, laoreet eu, sagittis id, ante.";
  "Etiam fermentum.";
  "Phasellus auctor enim eget sem.";
  "Morbi turpis arcu, egestas congue, condimentum quis, tristique cursus, leo.";
  "Sed fringilla.";
  "Nam malesuada sapien eu nibh.";
  "Pellentesque ac turpis.";
  "Nulla sed lacus.";
  "Mauris sed nulla quis nisi interdum tempor.";
  "Quisque pretium rutrum ligula.";
  "Mauris tempor ultrices justo.";
  "In hac habitasse platea dictumst.";
  "Donec sit amet enim.";
  "Suspendisse venenatis.";
  "Nam nisl quam, posuere non, volutpat sed, semper vitae, magna.";
  "Donec ut urna.";
  "Integer risus velit, facilisis eget, viverra et, venenatis id, leo.";
  "Cras facilisis felis sit amet lorem.";
  "Nam molestie nisl at metus.";
  "Suspendisse viverra placerat tortor.";
  "Phasellus lacinia iaculis mi.";
  "Sed dolor.";
  "Quisque malesuada nulla sed pede volutpat pulvinar.";
  "Cras gravida.";
  "Mauris tincidunt aliquam ante.";
  "Fusce consectetuer tellus ut nisl.";
  "Curabitur risus urna, placerat et, luctus pulvinar, auctor vel, orci.";
  "Class aptent taciti sociosqu ad litora torquent per conubia nostra, \
   per inceptos hymenaeos.";
  "Praesent aliquet, neque pretium congue mattis, \
   ipsum augue dignissim ante, ac pretium nisl lectus at magna.";
  "Vivamus quis mi.";
  "Nam sed nisl nec elit suscipit ullamcorper.";
  "Donec tempus quam quis neque.";
  "Donec rutrum venenatis dui.";
  "Praesent a eros.";
  "Aliquam justo lectus, iaculis a, auctor sed, congue in, nisl.";
  "Etiam non neque ac mi vestibulum placerat.";
  "Donec at diam a tellus dignissim vestibulum.";
  "Integer accumsan.";
  "Cras ac enim vel dui vestibulum suscipit.";
  "Pellentesque tempor.";
  "Praesent lacus.";
|]

let code_sample = [|
"/* One-sector disk I/O routine */

intx13:		movb 0x1(%si),%dh		# Load head
		movw 0x2(%si),%cx		# Load cylinder:sector
		movb $0x1,%al			# Sector count
		pushw %si			# Save
		movw %sp,%di			# Save
		testb $0x80,_FLAGS(%bp)		# Use packet interface?
		jz intx13.1			# No
		pushl $0x0			# Set the
		pushl 0x8(%si)			# LBA address
		pushw %es			# Set the transfer
		pushw %bx			#  buffer address
		push  $0x1			# Block count
		push  $0x10			# Packet size
		movw %sp,%si			# Packet pointer
		decw %ax			# Verify off
		orb $0x40,%ah			# Use disk packet
intx13.1:	int $0x13			# BIOS: Disk I/O
		movw %di,%sp			# Restore
		popw %si			# Restore
		retw				# To caller
";

"Cell *getnf(Node **a, int n)	/* get NF */
{
	if (donefld == 0)
		fldbld();
	return (Cell *) a[0];
}
";
|]

let shell_sample = [|
  (* A UUC *)
  "grep UUC < text",
  "This is an academic example of UUC.";

  (* An awkism *)
  "ls -l | awk '{ t = t + $5 } END {print(t)}'",
  "144065";

  (* Words *)
  "wc /usr/share/dict/words",
  "  234936  234936 2486813 /usr/share/dict/words";
|]


let text_to_title s =
  if String.length s > title_max then
    None
  else
    Some(String.sub s 0 (String.length s - 1))

let rec text_to_title_loop x l =
  match l with
    | [] -> x
    | (Some s) :: t -> text_to_title_loop (s::x) t
    | None :: t -> text_to_title_loop x t

let make_title a =
  Array.of_list (
    text_to_title_loop [] (List.map text_to_title (Array.to_list a))
  )

let title_sample =
  make_title text_sample

let title () =
  choose title_sample

let sentence () =
  choose text_sample

let paragraph () =
  let g = Random.int (para_max - para_min) + para_min in
  let b = Buffer.create (g + 80) in
    Buffer.add_string b (sentence ());
    while Buffer.length b < g do
      Buffer.add_char b ' ';
      Buffer.add_string b (sentence ());
    done;
    Buffer.contents b

let rec text_loop x n =
  if n > 0 then
    text_loop (paragraph()::x) (n-1)
  else
    x

let text () =
  let g = Random.int (text_max - text_min) + text_min in
    text_loop [] g

let code () =
  choose code_sample

let shell () =
  choose shell_sample
