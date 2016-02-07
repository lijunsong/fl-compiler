open Sexplib.Std
open Symbol

type label = Symbol.t with sexp
type temp = int with sexp

let temp_count = ref 0
let label_count = ref 0

let new_temp () =
  incr temp_count;
  !temp_count

let temp_to_string (tmp : temp) =
  "tmp" ^ (string_of_int tmp)

let label_with_prefix prefix =
  Symbol.of_string (prefix ^ (string_of_int !label_count))

let label_to_string label =
  Symbol.to_string label

let new_label ?(prefix="L") () =
  incr label_count;
  label_with_prefix prefix

let named_label name =
  Symbol.of_string name
