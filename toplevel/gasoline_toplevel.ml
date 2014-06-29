open Format

let gasolinemodule = [
  "appkit";
  "testkit";
]

let gasolineprinter = [
]


let directory_list () =
  match Gasoline_config.projectbase () with
  | Some s -> List.map (Filename.concat s) gasolinemodule
  | None -> [Gasoline_config.gasolinelibdir()]

let directory () =
  List.iter Topdirs.dir_directory (directory_list())

let install_printer () =
  let loop s =
    Topdirs.dir_install_printer Format.std_formatter (Longident.parse s)
  in
  List.iter loop gasolineprinter

let startup_hook () =
  let devel = true in
  begin
    directory();
    install_printer();
  end

let () =
  Toploop.toplevel_startup_hook := startup_hook
