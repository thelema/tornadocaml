(* non-functional list structure *)

type t
val new_list : int -> t
val singleton : int -> int -> t
val xor_list : t -> t -> unit
val remove_element : t -> int -> unit
val add_element : t -> int -> unit
val size : t -> int
val get_first : t -> int
val mem : t -> int -> bool
val to_list : t -> int list
val of_list : int -> int list -> t

(* only use the following if you're sure you're changing the element *)
val unsafe_remove_element : t -> int -> unit
val unsafe_add_element : t -> int -> unit
