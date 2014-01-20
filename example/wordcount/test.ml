let case (expected, s) =
  let got = Count.from_stream (Stream.of_string s) in
  got = expected

let suite = let open Count in [
  { bytes = 3; lines = 1; chars = 3; words = 2; longest = 1; }, "a\nb";
  { bytes = 4; lines = 2; chars = 4; words = 2; longest = 1; }, "a\nb\n";
  { bytes = 6; lines = 1; chars = 6; words = 3; longest = 1; }, "a\nbb c";
  { bytes = 6; lines = 1; chars = 6; words = 3; longest = 4; }, "aa b\nc";
]

let validate () =
  List.for_all case suite

let show () =
  List.map (fun (_,s) -> Count.from_stream (Stream.of_string s)) suite
