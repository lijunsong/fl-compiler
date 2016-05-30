open Batteries

type label = Symbol.t
type temp = int

let temp_count = ref 0
let label_count = ref 0

let new_temp () =
  incr temp_count;
  !temp_count

let temp_to_string (tmp : temp) =
  "r" ^ (string_of_int tmp)

let label_with_prefix prefix =
  Symbol.of_string (prefix ^ (string_of_int !label_count))

let label_to_string label =
  Symbol.to_string label

let new_label ?(prefix="L") () =
  incr label_count;
  label_with_prefix prefix

let named_label name =
  Symbol.of_string name

module LabelMap =
struct
  include Map.Make(Symbol)
  let find_opt x m =
    if mem x m then Some (find x m)
    else None
  let of_list lst =
    List.enum lst
    |> of_enum
end

module TempSet = Set.Make(struct
    type t = temp
    let compare = compare
  end)

module TempMap = Map.Make(struct
    type t = temp
    let compare = compare
  end)
