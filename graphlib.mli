(* Bipartite Graph handling library                                 *)
(* Written by Eric Norige                                           *)
(* This program is free software; you can redistribute it and/or    *)
(* modify it under the terms of the GNU Lesser General Public       *)
(* License as published by the Free Software Foundation; either     *)
(* version 2 of the License, or (at your option) any later version. *)
(*                                                                  *)
(* This program is distributed in the hope that it will be useful,  *)
(* but WITHOUT ANY WARRANTY; without even the implied warranty of   *)
(* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the    *)
(* GNU Lesser General Public License for more details.              *)

(** in general, modules of this type should raise exception if
 * anything funny happens, i.e. multiple insertions, removing invalid
 * keys, getting connections on non-existent nodes. *)

(*

exception Insert_collision

module type BpPart =
sig
  type t
 (** descriptor type of left nodes *)
  type x
 (** descriptor type of right nodes *)
  type y
 (** the empty map *)
  val empty : t
 (** returns a list of all left nodes *)
  val xnodes : t -> x list
 (** given a left node, return all right nodes it's connected to *)
  val get_conns_x : t -> x -> y list
 (** true if x is connected to y, false if not *)
  val mem : t -> x -> y -> bool 
 (** connects x to y.  Raises Insert_collision if this is already true *)
  val add : t -> x -> y -> t 
 (** removes all x to y connections.  raises Not_Found if none exist *)
  val remove : t -> x -> y -> t
 (** removes all connections from x.  Raises Not found if none exist *)
  val remove_x_node : t -> x -> t
end

module type BpCore =
sig
  include BpPart
  val edges : t -> int (* returns a count of connections *)
  val ynodes : t -> y list (* as BpPart.xnodes but for the right *)
  val get_conns_y : t -> y -> x list (* as BpPart.get_conns_x but for the right *)
  val remove_y_node : t -> y -> t (* as BpPart.remove_x_node but for the right *)
end

(*
module type BpCoreGen = 
  functor (X : Set.OrderedType) -> 
    functor (Y : Set.OrderedType) -> 
      BpCore

module type BpPartGen = 
  functor (X : Set.OrderedType) -> 
    functor (Y : Set.OrderedType) -> 
      BpPart

module BiGraphJoin :
  functor (XMaj : BpPart) ->
    functor (YMaj : BpPart with type x = XMaj.y and type y = XMaj.x) -> 
      BpCore

module BiGraphComplete : 
  functor (Bp : functor (X : Set.OrderedType) -> functor (Y : Set.OrderedType) -> 
	     BpPart with type x = X.t and type y = Y.t) -> BpCoreGen

module SparseBipartiteGraph1 : BpCoreGen

module SBGPart1 : BpPartGen

 *)
*)
