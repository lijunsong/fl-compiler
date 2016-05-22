(** This is a half baked implementation of a pretty printer.
*)
open Batteries

type doc =
  | Nil
  | Text of string * doc
  | Line of int * doc

let text s = Text (s, Nil)

let nil = Nil

let line = Line (0, Nil)

let rec nest i doc : doc = match doc with
  | Nil -> Nil
  | Text (t, doc') -> Text (t, nest i doc')
  | Line (j, doc') -> Line (i+j, nest i doc')

(** concatenate two docs *)
let rec (<->) doc doc' : doc = match doc with
  | Nil -> doc'
  | Text (t, d1) -> Text (t, d1 <-> doc')
  | Line (i, d1) -> Line (i, d1 <-> doc')

let concat sep docs : doc =
  let rec iter docs result =
    match docs with
    | [] -> result
    | [hd] -> hd <-> result
    | hd :: rest ->
      iter rest (hd <-> sep <-> result)
  in
  iter docs Nil

let rec layout = function
  | Nil -> ""
  | Text (s, d) -> s ^ (layout d)
  | Line (i, d) -> "\n" ^ (String.make i ' ') ^ (layout d)

let rec print_doc d =
  layout d |> print_string
