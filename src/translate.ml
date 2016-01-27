open Symbol

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
      name : Symbol.t;
      formals : access list;
      locals : mutable access list;
    }
  let new_frame (name : Symbol.t) (formals : bool list) : frame =
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

end
