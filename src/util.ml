open Batteries

let file_to_string f =
  let lines = File.lines_of f in
  String.join "\n" (List.of_enum lines)
