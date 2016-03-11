open Temp
open Batteries

type node = {
  temp: temp;
  mutable adj: TempSet.t;
}

type igraph = node list

val flow2igraph : Flow.flowgraph -> igraph

val to_string: igraph -> string
