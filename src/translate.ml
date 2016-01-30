open Symbol
open Batteries

module Temp = struct
  type label = Symbol.t
  type temp = int

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

end

module type Frame = sig
  type frame
  type access

  (** [new_frame name formals] create a frame named l. A list of
  bool indicates whether each formal argument escapes. *)
  val new_frame : Temp.label -> bool list -> frame

  (** retrieve the given frame's name *)
  val get_name : frame -> Temp.label

  (** retrieve the given frame's formal arguments.  *)
  val get_formals : frame -> access list

  (** [alloc_local f escape] allocate a local variable on frame [f] with
  [escape] indicating whether the variable escapes *)
  val alloc_local : frame -> bool -> access

end

module SparcFrame : Frame = struct
  type access =
    | InReg of Temp.temp   (** which register to store *)
    | InMem of int         (** offset in the frame *)

  type frame = {
      name : Temp.label;
      formals : access list;
      mutable locals : access list;
    }

  let new_frame (name : Temp.label) (formals : bool list) : frame =
    { name;
      formals = List.mapi (fun i f ->
                    if f then InMem((-4) * i) (* FIXME *)
                    else let t = Temp.new_temp() in
                         InReg(t)) formals;
      locals = [];
    }

  let count_locals = ref 0
  let get_name (fm : frame) = fm.name
  let get_formals (fm : frame) = fm.formals

  let alloc_local fm escape =
    incr count_locals;
    let loc = InMem(4 * !count_locals) in
    fm.locals <- loc :: fm.locals;
    loc
end

module F = SparcFrame

module Translate = struct

  type level = { parent : level option; frame : F.frame }

  type access = level * F.access

  let outermost = { parent = None;
                    frame = F.new_frame (Temp.new_label ~prefix:".main" ()) [];
    }

  let new_level parent label formals =
    let fm = F.new_frame label (true :: formals) in
    { parent = Some parent; frame = fm }

  (** get_formals will return the formal arguments of a
  function. (static link is implemented as an argument but not
  included.)*)
  let get_formals level : access list =
    let fm_formals = F.get_formals level.frame in
    match List.map (fun f -> level, f) fm_formals with
    | [] -> failwith "A level's formals cannot be empty list"
    | hd :: tl -> tl

  let get_label level =
    F.get_name level.frame

  let alloc_local level escape : access =
    let fm = level.frame in
    let fm_access = F.alloc_local fm escape in
    level, fm_access
end
