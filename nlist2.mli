(* functional version of nodelist *)

type t
val new_list : int -> t
val singleton : int -> int -> t
val xor_list : t -> t -> t
val remove_element : int -> t -> t
val add_element : int -> t -> t
val size : t -> int
val get_first : t -> int
val mem : int -> t -> bool
val to_list : t -> int list
val of_list : int -> int list -> t
val to_seq : t -> int Cf_seq.t

(* only use the following if you're sure you're changing the element *)
val unsafe_remove_element : int -> t -> t
val unsafe_add_element : int -> t -> t
