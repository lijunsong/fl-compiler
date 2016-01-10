open Batteries

type token =
  IF | THEN | ELSE | BEGIN | PRINT | END | SEMI | NUM | EQ

type buf = {buf : Buffer; curr : int}

let getToken buf : token = IF

let advance buf : unit = ()

let eat buf t : unit =
  if getToken buf <> t then
    error "unexpected token"
  else
    advance buf

let S buf : unit = match getToken buf with
  | IF -> begin
      eat buf IF;
      E buf;
      eat buf THEN;
      S buf;
      eat buf ELSE;
      S buf
    end
  | BEGIN -> begin
      S buf;
      L buf
    end
  | PRINT -> begin
      E buf
    end
  | _ -> error "unexpected token in S"

let L buf = match getToken buf with
  | END -> eat buf END
  | SEMI -> begin
      eat buf SEMI;
      S buf;
      L buf
    end
  | _ -> error "unexpected token in L"

let E buf = begin
  eat buf NUM;
  eat buf EQ;
  eat buf NUM
  end


(** main *)
let usage () =
  let prog = Sys.argv.[0] in
  Printf.printf "%s [an optional file]" prog

let _ =
  let input =
    if Array.length Sys.argv = 1 then
      open_in Sys.argv.[0]
    else
      stdin
  in
