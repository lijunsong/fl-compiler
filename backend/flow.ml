open Assem
open Batteries

type node = {
  def : Temp.temp list;
  use : Temp.temp list;
  ismove : bool;
  mutable succ: node list;
  mutable pred: node list;
  mutable live_out:  Temp.temp list;
}

type flowgraph = node list

let instr2node instr = match instr with
  | OP (ass, def, use, jump) ->
    {def; use; ismove=false; succ=[]; pred=[]; live_out=[]}
  | LABEL (ass, l) ->
    {def=[]; use=[]; ismove=false; succ=[]; pred=[]; live_out=[]}
  | MOVE (ass, dst, src) ->
    {def=[dst]; use=[src]; ismove=false; succ=[]; pred=[]; live_out=[]}

let instrs2graph instrs =
