open Format

let gasolinemodule = [
  "appkit";
  "testkit";
]

let gasolineprinter = [
]

let gasolinesrcdir () =
  "/usr/home/michael/Workshop/gasoline"

let gasolinelibdir () =
  "/usr/local/lib/ocaml/site-lib/gasoline"

let directory devel =
  let makedir subdir =
    Filename.concat (gasolinesrcdir()) subdir
  in
  if devel then
    List.iter (fun s -> Topdirs.dir_directory(makedir s)) gasolinemodule
  else
    Topdirs.dir_directory (gasolinelibdir())

let install_printer () =
  let loop s =
    Topdirs.dir_install_printer Format.std_formatter (Longident.parse s)
  in
  List.iter loop gasolineprinter

let startup_hook () =
  let devel = true in
  begin
    directory devel;
    install_printer();
  end

let () =
  Toploop.toplevel_startup_hook := startup_hook
