open Format

let gasolinemodule = [
  "appkit";
  "testkit";
]

let gasolineprinter = [
  "Unicode.UChar.printer";
  "Unicode.UString.printer";
  "CValue.printer";
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

let headline () =
  match Gasoline_config.projectbase () with
  | Some(_) -> printf "Runnning Gasoline toplevel in developer mode\n%!"
  | None -> ()

let startup_hook () =
  begin
    headline();
    directory();
    install_printer();
  end

let () =
  Toploop.toplevel_startup_hook := startup_hook
