open Batteries

let file_to_string f =
  let lines = File.lines_of f in
  String.join "\n" (List.of_enum lines)

(** return a tuple where first element is the given list without the
    last element, the second element is the last element.

    @raise [Failure] if the list is empty.
*)
let split_last lst =
  let rec split part1 part2 = match part2 with
    | [] -> failwith "empty list"
    | hd :: [] ->
      List.rev part1, hd
    | hd :: rest ->
      split (hd :: part1) rest in
  split [] lst
