(** A polymorphic filter node.
    The user supplies a predicate; only elements that satisfy it are kept. *)
type 'a filter =
  { input : 'a list (** Elements to test *)
  ; pred : 'a -> bool (** Keep the element if pred x = true *)
  }

(** Execute the filter node and return the surviving elements. *)
let run_filter (node : 'a filter) : 'a list = List.filter node.pred node.input
