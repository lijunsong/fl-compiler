(** Temp are abstract names for local variables *)

open Symbol
open Batteries

type label
(** abstract names for static memory location *)


type temp
(** a value temporarly held in registers. We assume that we have
    infinite number of registers *)

val new_temp : unit -> temp

val temp_to_string : temp -> string

val new_label : ?prefix:string -> unit -> label

val label_to_string : label -> string

val named_label : string -> label
(** Returns a new label whose assembly language name is the given
    string. This is useful for debugging *)

module LabelMap : BatMap.S with type key = label

module TempSet : BatSet.S with type elt = temp

module TempMap : BatMap.S with type key = temp
