open Temp
open Batteries

type status =
  | Ingraph of int
  | Removed
  | Colored of string

module rec Node : sig
  type t = {
    temp: temp;
    mutable adj: NodeSet.t;
    mutable status : status ref;
  }
  val compare : t -> t -> int
end
and NodeSet : (Set.S with type elt = Node.t)

type igraph = Node.t list

val decrease_degree : Node.t -> unit

val increase_degree : Node.t -> unit

val flow2igraph : Flow.flowgraph -> igraph

val to_string: igraph -> string
