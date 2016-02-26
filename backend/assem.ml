type temp = Temp.temp

type label = Temp.label

type instr =
  | OP of string * temp list * temp list * label list option
  (** assembly, dst, src, jump *)

  | LABEL of string * label
  (** assembly, label *)

  | MOVE of string * temp * temp
  (** assembly, dst, src *)
