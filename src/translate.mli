(** Temp are abstract names for local variables *)
module Temp : sig

  (** abstract names for static memory location *)
  type label

  (** a value temporarly held in registers, which we assumed to be
  infinite *)
  type temp

  val new_temp : unit -> temp

  val temp_to_string : temp -> string

  val new_label : ?prefix:string -> unit -> label

  val label_to_string : label -> string

  (** Returns a new label whose assembly language name is the given
  string *)
  val named_label : string -> label

  (** Returns a new label whose prefix is the given string but with a
  suffix random number *)
  val prefixed_label : string -> label

end

(** *)
module type Frame = sig
  type frame
  type access

  (** [new_frame name formals] create a frame named l. A list of
  bool indicates whether each formal argument escapes. *)
  val new_frame : Temp.label * bool list -> frame

  (** retrieve the given frame's name *)
  val get_name : frame -> Temp.label

  (** retrieve the given frame's formal arguments.  *)
  val get_formals : frame -> access list

  (** [alloc_local f escape] allocate a local variable on frame [f] with
  [escape] indicating whether the variable escapes *)
  val alloc_local : frame -> bool -> access

end

(** TODO: make translate a functor taking Frame as input module *)
module Translate : sig
  (** Each nested function declared in Tiger's [let] is in a deeper
  level *)
  type level

  type access

  val outermost : level

  (** [new_level parent name formals ] create a new level for a nested
  function. *)
  val new_level : level -> Temp.label -> bool list -> level

  val get_formals : level -> access list

  val get_label : level -> Temp.label

  (** [alloc_local level escape] allocate a local variable on [level]
  with [escape] indicating whether the variable escapes. *)
  val alloc_local : level -> bool -> access
end
