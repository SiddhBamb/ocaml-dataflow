(** A polymorphic computation node.
    Users specifies a function to apply on each element of a list. *)
type ('a, 'b) t =
  { input : 'a list (** Elements to process *)
  ; transform : 'a -> 'b (** User-supplied transformation *)
  }

(** Run the node and get the outputs. *)
let run_computation (node : ('a, 'b) t) : 'b list = List.map node.transform node.input
