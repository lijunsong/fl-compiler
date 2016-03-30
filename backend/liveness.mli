open Temp
open Batteries

type status =
  | Ingraph of int
  | Removed
  | Colored of string

type node = {
  temp: temp;
  mutable adj: TempSet.t;
  mutable status : status ref;
}

type igraph = node list

val flow2igraph : Flow.flowgraph -> igraph

val to_string: igraph -> string
