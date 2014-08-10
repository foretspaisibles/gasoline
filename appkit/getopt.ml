(* Getopt -- Program arguments analysis

Author: Michael Grünewald
Date: Sun  4 May 2008 11:08:10 CEST

Copyright © 2008-2012 Michael Grünewald

This file must be used under the terms of the CeCILL-B.
This source file is licensed as described in the file COPYING, which
you should have received as part of this distribution. The terms
are also available at
http://www.cecill.info/licences/Licence_CeCILL-B_V1-en.txt *)

open Printf
let buffer_sz = 64
let progname = Filename.basename Sys.executable_name

(* Le module [Getopt] offre des services dans le domaine de
l'interface utilisateur, pour l'analyse de la ligne de commande. Ces
services comprennent l'analyse et le traitement de la ligne de
commande et l'affichage d'un dialogue d'aide.

Sur les systèmes UNIX, les programmes sont éxécutés à partir d'un
shell, et des options de démarrage peuvent être précisées sur la ligne
de commande pour induire des comportements particuliers du programme,
ou simplement lui indiquer quels sont les fichiers qu'il doit traiter,
etc. Les nombreux programmes existant utilisent des conventions
différentes pour lire les options de la ligne de commande. On peut par
exemple lire les pages de manuel de tar(1), sox(1) et sort(1). Le
programme tar(1) utilise une convention ancienne, qu'il est
aujourd'hui le seul à utiliser. Le programme sox(1) a une ligne de
commande adaptée à son but particulier. Le programme sort(1) est un
programme typique, il adopte la convention d'analyse de ligne de
commande la plus ordinaire. Le module [Getopt] permet aux programmes
clients d'utiliser cette convention, que nous décrivons en détail
ci-dessous.

Les services rendus par le module [Getopt] analysent un vecteur
d'arguments, par exemple le vecteur [Sys.argv], en fonction d'une
spécification de la ligne de commande. Cette spécification détaille
quelles sont les options autorisées, quelles sont celles qui
nécessitent ou ne nécessitent pas d'argument, et ce qu'il faut faire
des arguments qui ne sont pas des options. La spécification précise en
outre une petite documentation du programme, que l'utilisateur peut
demander à voir avec l'option d'aide.

Dans ce module, on commence par donner des définitions adaptée à la
description de la ligne de commande. On définit ensuite des fonctions
adaptées à l'impression des messages d'aide. Enfin, vient la
réalisation des fonctions du module
*)

(*s On ne donne pas les définitions de la façon la plus formelle, mais
on produit quelques exemples typiques d'emploi de la ligne de
commandes. On suppose que le programme \emph{prog} propose les options
décrites dans le tableau suivant:
\begin{center}
\begin{tabular}{cp{.7\textwidth}}
\emph{option}&\emph{description}\\
\tabularhline
\emph{h}&le programme affiche le message d'aide et termine\\
\emph{v}&le programme affiche plus de message sur soun déroulement
  qu'à l'ordinaire\\
\emph{c}~$n$&le programme utilise~$n$ comme nombre d'itérations au
  lieu de 10, habituellement\\
\emph{o}~$f$&le programme inscrit les résultats du traitement dans le
  fichier ~$f$ au lieu de les retranscrire sur la sortie standard\\
\end{tabular}
\end{center}
Le programme prend en outre comme arguments supplémentaires une liste
de noms de fichiers contenant les données à lire, lorsque cette liste
est vide, il lit les données sur l'entrée standard. Voici la forme normale:
\begin{verbatim}
prog -a -x 12 -o output file1 file2
\end{verbatim}
Dans l'exemple suivant, le vecteur des arguments est formé de
\begin{description}
\Item{Un commutateur (\emph{flag}) -a} dont le rôle est d'activer un
  certain comportement, le commutateur est une option sans argument;
\Item{Deux paramètres (\emph{para}) -x 12 et -o output} dont le
  rôle est de modifier une variable interne du programme, les
  paramètre sont des options avec argument;
\Item{Des arguments supplémentaires (\emph{rest}) file1 et file2}
  dont le rôle est de déterminer l'ensemble des données à traiter.
\end{description}
La ligne de commande aurait pu être écrite de l'une quelconque des
manières suivantes:
\begin{verbatim}
prog -a -x12 -ooutput file1 file2
prog -ax12 -ooutput file1 file2
prog -a -x 12 -o output -- file1 file2
\end{verbatim}
La séquence \texttt{--} permet de séparer les options des arguments,
cela est utile si par exemple il fallait traiter un fichier dont le
nom commence par \texttt{-o}.
*)

(*s Commençons par examiner les conditions exceptionnelles pouvant se
produire lors de l'analyse et de l'interpétation de la ligne de
commande.

Lorsque l'utilisateur ne demande pas une exécution
ordinaire du programme mais souhaite afficher l'aide, le callback de
l'option d'aide lève l'exception [Help], cette exception est remarquée
par la fonction supervisant l'interpétation de la ligne de commande
qui interrompt cette interprétation et déclenche la procédure
affichant l'aide. *)
exception Help

(* Les autres situations exceptionnelles pouvant être rencontrées sont
celles où la ligne de commande examinée est mal formée. Ces
malformations sont de trois sortes.
\begin{description}
\Item{Option inconnue}
qui survient lorsqu'un caractère ne représentant pas une option est
rencontré dans un \emph{cluster} d'options;
\Item{Séparation des options et des arguments supplémentaires}
qui survient lorsque le bloc spécial représentant la fin des options
apparaît ailleurs qu'à la fin d'un \emph{cluster}
\Item{Argument absent}
qui survient lorsqu'une option nécessite un argument et que celui-ci
n'est pas fourni dans le vecteur d'arguments;
\Item{Argument ne pouvant être converti}
qui se produit lorsqu'une option attend un argument d'un type ne
pouvant être obtenu à partir de la chaîne de caractères donnée.
\end{description}
Nous definissons ici une exception [Error] et un type [error] ainsi
que des fonctions ancillaires pour les manipulations simples de ces
exceptions.


Le type [error] est un type somme avec trois constructeurs. Le premier
[ESHORT] mémorise l'option courte dont le traitement a provoqué une
erreur et un message, le second [ELONG] mémorise l'option longue sont
le traitement a provoqué l'erreur, et un message.*)
type error =
    | ESHORT of char * string
    | ELONG of char * string * string
    | ESEPARATOR


let error_to_string = function
  | ESHORT(c,s) -> sprintf "%s -- %s" s (Char.escaped c)
  | ELONG(c,o,s) -> sprintf "%s -- %s %s" s (Char.escaped c) o
  | ESEPARATOR -> "illegal double dash construction"

exception Error of error


(* Les fonctions [error_*] servent à lever l'exception [Error].*)
let error_illegal_short c =
  raise(Error(ESHORT(c,"illegal option")))

let error_illegal_long c o =
  raise(Error(ELONG(c,o,"illegal option")))

let error_argument_short c =
  raise(Error(ESHORT(c,"option requires an argument")))

let error_argument_long c o =
  raise(Error(ELONG(c,o,"option requires an argument")))

let error_separator () =
  raise(Error ESEPARATOR)

let error_convert_short c f v =
  raise(Error(ESHORT(c,sprintf "option argument %S let %s fail" v f)))

let error_convert_long c o f v =
  raise(Error(ELONG(c,o,sprintf "option argument %S let %s fail" v f)))


(*s Nous définissons maintenant les types de données représentant les
paramètres de l'interprétation de la ligne de commande. Ces paramètres
sont rassemblés dans une structure [spec] qui est un argument de la
fonction [parse] effectuant l'analyse de la ligne de commandes.

Le type [t] décrit l'effet d'une option. Cette description est formée
de: [option] la lettre représentant l'option sur la ligne de commande,
[help] une chaîne de caractères décrivant l'aide sur la ligne de
commande, [callback] la fonction à appeler sur l'argument de l'option,
et [wants_arg] un commutateur indiquant si [callback] attend
effectivement un argument. *)
type t = {
    option: char;
    help: string;
    callback: string -> unit;
    wants_arg: bool;
}


(* Le type [note] définit des notes supplémentaires pouvant être
affichée lorsque l'aide du programme est demandée, il peut s'agir
d'une notice de copyright, ou de tout autre chose. *)
type note = {
    title: string;
    content: string;
}

(* Le type [spec] renferme une spécification complète de la ligne de
commande. L'interpr'etation de la ligne de commandes est une fonction
[spec -> string array -> unit].*)
type spec = {
    usage: string;
    description: string;
    options: t list;
    rest: string -> unit;
    notes: note list;
}

type 'a reader = string -> 'a

(* Nous définissons des créateurs pour le type [t], qui fournissent
une interface confortable à l'utilisateur. Ces créateurs prennent en
argument le caractère représentant l'option, la chaîne d'aide est un
callback dont le type est indiqué par le nom du créateur. La fonction
[option_factor] factorise la création d'une option avec paramètre.*)


let supervise_convert_short c f v =
  try f v
  with Failure m -> error_convert_short c m v

let flag o c d = {
    option = o;
    help = d;
    callback = (fun _ ->  c ());
    wants_arg = false;
}

let option_factor f o c d = {
    option = o;
    help = d;
    callback = (fun x -> c (supervise_convert_short o f x));
    wants_arg = true;
}

let char_of_string s =
  if String.length s != 1 then
    failwith "char_of_string"
  else
    s.[0]

let concrete_of_string reader s =
  try reader s
  with _ -> failwith "concrete_of_string"

let char = option_factor char_of_string
let bool = option_factor bool_of_string
let string = option_factor (function x -> x)
let int = option_factor int_of_string
let float = option_factor float_of_string
let concrete conc = option_factor (concrete_of_string conc)

let note t c = {
    title = t;
    content = c;
}

let spec u d o r n = {
    usage = u;
    description = d;
    options = o;
    rest = r;
    notes = n;
}


(*s Nous réalisons à présent les fonctions nécessaires à l'impression
du message d'aide. Le résultat que l'on souhaite obtenir doit
ressembler au suivant:
\begin{verbatim}
Usage: eps2png [-r resolution] [-f png_flavour] [-h] infile [outfile]
 Convert from Encapsulated PostScript to Portable Network Graphics
Options:
 -r resolution [320x320]
    Select resolution for the resulting image. The <resolution> can
    follow one of the two following formats: '<xdpi>x<ydpi>' or
    '<dpi>'. In the latter case, the value <dpi> is used for both
    horizontal and vertical resolution.
 -f png_flavour [alpha]
    The flavour'' is one of the following:  'alpha', 'gray', '16',
    and '256'. The purpose of this option is to select the actual
    GHOSTSCRIPT driver to be used by eps2png.
 -h Provide the user with an help message and gracefully exit.
Notes:
 The conversion is done thanks to ghostscript.
 Ghostscript is run in SAFER mode (see ghostscript documentation).
Author: Michael Grünewald <michael.grunewald@laposte.net>
Copyright: (c)2005
\end{verbatim}
Le message d'aide est formé d'une suite d'alinéas portant un titre, et
au sein d'un alinéa les paragraphes sont imprimés avec un retrait de
un caractère. Lorsque l'alinéa ne contient qu'un paragraphe pouvant
tenir sur une seule ligne, comme \texttt{Author} ci-dessus, ce
paragraphe est imprimé immédiatement à la suite du titre. Lorsque
l'alinéa est composé de plusieurs paragraphes ou qu'il est formé d'un
long paragraphe, le titre est placé sur sa propre ligne.

Dans les chaînes de caractères [t.help] et [note.content] les
caractères~CR indiquent un nouveau paragraphe.

Les alinéas \texttt{Usage} et \texttt{Options} sont préparés avec une
procédure spéciale. Dans le paragraphe \texttt{Usage} aucune coupure
de ligne ne peut survenir entre deux crochets.

Pour produire ces effets, on utilise le module [Format] qui fournit
des services de \emph{prettyprinter}. Les définitions nécessaires ne
présentent pas d'intérêt pour le reste du module, et leur définition
n'est pas reproduite ici, à part pour les deux fonctions
[Helper.message] et [Helper.usage] procédant effectivement à
l'affichage des messages. Il est à noter que la première de ces
fonctions affiches ses messages sur la sortie standard et la seconde
sur l'erreur standard. *)



module Help =
struct
  (*i*)
  open Format

  let alinea_open title =
    print_cut ();
    open_hvbox 1;
    print_string title;
    print_char ':';
    print_space ()

  let alinea_close () =
    close_box ()

  let paragraph_open () =
    open_box 0

  let paragraph_close () =
    close_box ()

  let paragraph_switch () =
    paragraph_close ();
    print_cut ();
    paragraph_open ()

  let paragraph_insert text =

    let l = String.length text in
      for i = 0 to l - 1 do
	match text.[i] with
	  | '\n' -> paragraph_switch ()
	  | ' ' -> print_space ()
	  | c -> print_char c
      done

  let compose_note note =

    alinea_open note.title;
    paragraph_open ();
    paragraph_insert note.content;
    paragraph_close ();
    alinea_close ()


  let usage_open () =
    open_hovbox 1;
    print_string "Usage:";
    print_space ()

  let usage_close () =
    close_box ();
    print_cut ()

  let usage_open_bracket () =
    print_cut();
    open_hbox ();
    print_char '['

  let usage_close_bracket () =
    print_char ']';
    close_box ();
    print_cut ()


  let usage_insert text =

    let l = String.length text in
      for i = 0 to l - 1 do
	match text.[i] with
	  | '\n' -> failwith "usage_insert_text: newlines are not allowed here"
	  | ' ' -> print_space ()
	  | '[' -> usage_open_bracket ()
	  | ']' -> usage_close_bracket ()
	  | c -> print_char c
      done

  let compose_usage usage description =

    usage_open ();
    usage_insert usage;
    usage_close ();
    open_hbox ();
    print_char ' ';
    print_string description;
    close_box ();
    print_cut ()

  let option_open option =
    print_cut();
    open_vbox 3;
    print_char '-';
    print_char option;
    print_char ' '

  let option_close () =
    close_box()

  let option_newline () =
    close_box ();
    print_cut ();
    open_box 0

  let option_insert text =
    let l = String.length text in
      for i = 0 to l - 1 do
	match text.[i] with
	  | '\n' -> option_newline ()
	  | ' ' -> print_space ()
	  | c -> print_char c
      done

  let compose_option option =
    option_open option.option;
    open_box 0;
    option_insert option.help;
    close_box ();
    option_close()
(*i*)
  let message spec =
    open_vbox 0;
    compose_usage (progname ^ " " ^ spec.usage) spec.description;
    open_vbox 1;
    print_string "Options:";
    List.iter compose_option spec.options;
    close_box();
    List.iter compose_note spec.notes;
    print_cut();
    close_box()


  let usage spec =
    Format.set_formatter_out_channel stderr;
    open_vbox 0;
    usage_open ();
    usage_insert (progname ^ " " ^ spec.usage);
    usage_close ();
    close_box()

end


(*s L'analyse de la ligne de commande utilise une présentation
abstraite de la ligne de commande. La première abstraction montre une
vecteur d'argument comme un flot de caractères, une marque étant
insérée entre les éléments du vecteur. La seconde abstraction découpe
ce dernier flot en couples [(f,s)] où [f] est une fonction et [s]
l'argument surlaquelle l'appliquer évaluer l'option correspondante. *)


module OptionList =
struct

  (* La fonction[has_option] est un prédicat pour le type [t], qui
  teste un caractère contre la valeur du paramètre [option]. *)
  let has_option c x =
    x.option = c

  let is_option f c =
    List.exists (has_option c) f

  let wants_arg f c =
    (List.find (has_option c) f).wants_arg

  let callback f c =
    (List.find (has_option c) f).callback

end


module Level1 =
struct

  type t =
      | CHAR of char
      | BOUNDARY

  let from_argv_sub v =

    let l = Array.length v in
    let c = ref 0 in
    let m = ref 0 in
    let j = ref 0 in
    let _ = (
	if l > 0 then
	  m := String.length v.(0)
    ) in

      function index ->

	if !c = l then
	  None
	else if !j = !m then (
	    incr c;
	    j := 0;
	    if !c < l then
	      m := String.length v.(!c);
	    Some BOUNDARY

	) else (
	    incr j;
	    Some(CHAR(v.(!c).[!j - 1]))
	)

  let print x =
    match x with
      | BOUNDARY -> printf "\n"
      | CHAR c -> printf "%c" c

  let dump = Stream.iter print

  let from_argv v =
    Stream.from (from_argv_sub v)

  let is_at_boundary s =

    match Stream.peek s with
      | Some BOUNDARY -> true
      | _ -> false

  let maybe_discard_boundary s =

    if is_at_boundary s then Stream.junk s


  let iter_to_boundary s f =

    while not(is_at_boundary s) do
      match Stream.next s with
	| CHAR c -> f c
	| _ -> failwith "Level1.iter_to_boundary: this never happens"
    done;
    maybe_discard_boundary s


  let slurp_to_boundary s =

    let b = Buffer.create buffer_sz in
      iter_to_boundary s (Buffer.add_char b);
      Buffer.contents b

  let is_empty s =
    match Stream.peek s with
      | None -> true
      | _ -> false

end


module Level2 =
struct

  open Level1
  open OptionList

  let from_level1_sub l r l1 =

    let state = ref (fun _ -> failwith "this never happens") in

    let rec reading_rest _ =
      if is_empty l1
      then (
	  state := reading_finished;
	  None
      ) else Some(r, slurp_to_boundary l1)

    and reading_finished _ =
      None

    and reading_option _ =
      if is_empty l1
      then(
	  state := reading_finished;
	  None
      ) else (
	  match Stream.next l1 with
	    | CHAR '-' -> process_hyphen ()
	    | CHAR c -> process_char c
	    | BOUNDARY -> process_boundary ()
      )

    and process_boundary () =
      state := reading_rest;
      Some(r, "")

    and process_char c =
      state := reading_rest;
      Some(r, (String.make 1 c) ^ (slurp_to_boundary l1))

    and process_hyphen () =
      state := reading_cluster_continue;
      reading_cluster_start ();

    and reading_cluster_start _ =
      match Stream.next l1 with
	| CHAR '-' -> process_double_hyphen ()
	| CHAR c -> process_option c
	| BOUNDARY -> process_lone_hyphen ()

    and reading_cluster_continue _ =
      match Stream.next l1 with
	| CHAR '-' -> process_double_hyphen ()
	| CHAR c -> process_option c
	| BOUNDARY -> reading_option 0

    and process_lone_hyphen () =
      (* Nous avons rencontré un caractère \texttt{-} isolé qui
      indique la fin des options et constitue lui-même un argument. *)
      state := reading_rest;
      Some(r, "-")

    and process_double_hyphen () =
      state := reading_rest;
      match Stream.next l1 with
	| CHAR _ -> error_separator ()
	| BOUNDARY -> reading_rest 0

    and process_option c =

      if is_option l c
      then
	if wants_arg l c
	then (
	    state := reading_option;
	    maybe_discard_boundary l1;
	    try Some(callback l c, slurp_to_boundary l1)
	    with Stream.Failure -> error_argument_short c
	) else (
	    Some(callback l c, "")
	)
      else
	error_illegal_short c

    in

      state:= reading_option;
      function x -> !state x

  let from_level1 l r l1 =
    Stream.from (from_level1_sub l r l1)

end

(*s Les modules [Level1] et [Level2] permettent une lecture abstraite
de la ligne de dommande. Maintenant qu'ils sont définis on peut
réaliser la fonction [parse] qui est la fonction principale dans
l'interface du module [Getopt].

La fonction [parse] a comme argument une spécification [spec] pour
l'analyse de la ligne de commande, et un vecteur d'arguments à
analyser. En premier lieu, la fonction procède à quelques ajustements
et notamment fournit le cas échéant une option d'aide. En second lieu,
la ligne de commande est analysée grâce aux fonctions définies dans
[Level1] et [level2]. Enfin, les arguments sont traités.
*)

let provide_help spec =

  if OptionList.is_option spec.options 'h'
  then spec
  else { spec with options = spec.options @ [{
      option = 'h';
      wants_arg = false;
      help = "Displays an help message and gracefully exits.";
      callback = fun _ -> raise Help;
  }]}

let run_help spec =
  Help.message spec;
  exit 0

let run_usage err spec =
  eprintf "%s: %s\n" progname (error_to_string err);
  Help.usage spec;
  exit 64 (* See sysexits(3) *)

let parse_apply (a,b) =
  a b

let parse spec argv =

  let s = provide_help spec in

  try

    let l1 = Level1.from_argv argv in
    let l2 = Level2.from_level1 s.options s.rest l1 in

      Stream.iter parse_apply l2

  with
    | Help -> run_help s
    | Error e -> run_usage e s

let parse_argv spec = parse spec
  (Array.of_list (List.tl (Array.to_list Sys.argv)))

(* Examinons maintenat le cas où l'on souhaite fournir des
noms longs pour les options, cette démarche a pour intérêt
d'affranchir les programmes complexes de la limitation portant sur le
nombre des options pouvant être présentées à l'utilisateur. Cette
méthode est utilisée dans deux programmes très courants, \emph{tar(1)}
et~\emph{gcc(1)}.

Le programme \emph{tar(1)} utilise l'option \texttt{W} qui accepte un
grand nombre d'arguments comme \texttt{check-links} ou
\texttt{exclude=pattern}. Le programme \emph{gcc(1)} utilise de
nombreuses constructions de ce genre, l'option \texttt{W} permet
d'activer ou de désactiver les messages d'avertissement, l'option
\texttt{f} permet de règler les paramètres de la passe d'optimisation,
l'option \texttt{m} sélectionne des options particulières à la machine
sur laquelle doit être exécuté le programme compilé.

*)

type long_option = {
    long_option: string;
    long_callback: string -> unit;
    long_wants_arg: bool;
}

(* La fonction [supervise_convert_long] rattrape les erreurs de
conversion dans le traitement des arguments des options longues. Elle
est utilisée dans un contexte où l'option courte en court de
traitement est inconnue et utilise ['?'] comme valeur pour le nom de
cette option courte. La véritable valeur est insérée à cet endroit par
la fonction [long_option_callback].*)
let supervise_convert_long o f v =
  try f v
  with Failure m -> error_convert_long '?' o m v

let long_flag o c = {
    long_option = o;
    long_callback = (function _ -> c ());
    long_wants_arg = false;
}

let long_factor f o c = {
    long_option = o;
    long_callback = (function x -> c (supervise_convert_long o f x));
    long_wants_arg = true;
}

let long_char = long_factor char_of_string
let long_bool = long_factor bool_of_string
let long_string = long_factor (function x -> x)
let long_int = long_factor int_of_string
let long_float = long_factor float_of_string
let long_concrete conc = long_factor (concrete_of_string conc)

let long_option_get_name s =

  try

    let i = String.index s '=' in
      String.sub s 0 i

  with Not_found -> s

let long_option_has_name n x =
  n = x.long_option


let long_option_callback o f s =

  let l = String.length s in
  let n = long_option_get_name s in
  let m =
    try List.find (long_option_has_name n) f
    with Not_found -> error_illegal_long o n
  in

    if m.long_wants_arg
    then

      let i =
	try String.index s '='
	with Not_found -> error_argument_long o n
      in
      let a = String.sub s (i+1) (l-i-1) in


	try m.long_callback a
	with Error(ELONG(_,n,m)) -> raise(Error(ELONG(o,n,m)))

    else

      m.long_callback ""



let long o c d = {
    option = o;
    callback = long_option_callback o c;
    help = d;
    wants_arg = true;
}

let store r v =
  r := v

let set v r () =
  r := v

let queue a v =
  a := !a @ [v]
