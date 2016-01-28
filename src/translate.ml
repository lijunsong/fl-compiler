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

  let label_with_prefix prefix =
    Symbol.of_string (prefix ^ (string_of_int !label_count))

  let new_label () =
    incr label_count;
    label_with_prefix "L"

  let named_label name =
    Symbol.of_string name

  let prefixed_label name =
    incr label_count;
    label_with_prefix name
end

module SparcFrame : Frame = struct
  type access =
    | inReg of Temp.temp   (** which register to store *)
    | inMem of int         (** offset in the frame *)

  type frame = {
      name : Temp.label;
      formals : access list;
      locals : mutable access list;
    }
  let new_frame (name : Temp.label) (formals : bool list) : frame =
    { name;
      formals = List.map (fun f ->
                    if f then inMem(0) (* FIXME *)
                    else let t = Temp.new_temp() in
                         inReg(t)) formals
    }

  let get_name (fm : frame) = fm.name
  let get_formals (fm : frame) = fm.formals

  let alloc_local fm escape =
    (* FIXME *)
    let loc = inMem(0) in
    fm.locals <- loc :: fm.locals;
    loc
end

module Translate = struct
  type access = level * Frame.access

  type level = { parent : level option; frame : Frame.frame }

  let outermost : level = {
      parent = None;
      Frame.new_frame (Temp.prefixed_label ".main") []
    }

  let new_level parent label formls =
    let fm = Frame.new_frame label (true :: formals) in
    { Some parent; fm }

  let get_formals level =
    let fm = level.frame in
    let fm_formals = Frame.get_formals level.frame in
    level, fm_formals

  let alloc_local level escape : access =
    let fm = level.frame in
    let fm_access = Frame.alloc_local fm escape in
    level, fm_access
end
