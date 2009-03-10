exception Constraint_Error
    
type 'a core = {
    pos : 'a -> int;
    value : int -> 'a;
    compare : 'a -> 'a -> int;
  }

type 'a range = {
    min : 'a;
    max : 'a;
  }
  
module type DiscreteCore = sig
  type t
  val pos : t -> int
  val value : int -> t (* throws Constraint_Error if not valid position *)
  val compare : t -> t -> int
  val image : t -> string
end

module Int = struct 
  type t = int 
  let pos x = x
  let value x = x
  let compare x y = x-y
  let image x = string_of_int x
end

module type Range = sig
  type t
  val min : t
  val max : t
(* min must be less than max *)
end

module NaturalRange = struct
  type t = int
  let min = 0
  let max = max_int
end

module PositiveRange = struct
  type t = int
  let min = 1
  let max = max_int
end

module type DiscreteRange = sig
  type t
  val compare : t -> t -> int
  val first : t
  val last : t
  val range : t range
  val width : int
  val min : t -> t -> t
  val max : t -> t -> t
  val succ : t -> t
  val pred : t -> t
  val pos : t -> int
  val value : int -> t
  val image : t -> string
end

module GenDR 
    (DC: DiscreteCore) 
    (R : Range with type t = DC.t) : DiscreteRange = 
  struct
    type t = DC.t
    let compare = DC.compare
    let min_pos = DC.pos R.min
    let max_pos = DC.pos R.max
	
    let first = R.min
    let last = R.max
    let range = {min = R.min; max = R.max}
    let min x y = if DC.compare x y > 0 then y else x
    let max x y = if DC.compare x y > 0 then x else y
    let fpos = DC.pos first
    let pos x = DC.pos x - fpos	
    let width = pos last
    let value p =  
      if p >= width or p < 0 
      then raise Constraint_Error 
      else DC.value (p+fpos)
    let succ x = value (succ (pos x))
    let pred x = value (pred (pos x))
    let image x = DC.image x
  end

module IntRng = GenDR(Int)

module Natural = IntRng(NaturalRange)
module Positive = IntRng(PositiveRange)
