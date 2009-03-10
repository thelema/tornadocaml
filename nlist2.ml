module Functional = 
  struct
    
    type t = int option * int * Bitv.t
	  
    let new_list n = (Some 0, max_int, Bitv.create n false)
	
    let singleton n i = 
      assert (0 <= i && i < n);
      let l = Bitv.create n false in
      Bitv.set l i true;
      (Some 1, i, l)

    let size = function
	(Some c, _, _) -> c
      | (None, _, vec) -> List.length (Bitv.to_list vec)
	    
    let get_first vec pos =   (* returns the first true in vector v after (or at) v.(pos) or max_int if none*)
      let len = Bitv.length vec in
      let rec gf_rec p =
	if p >= len 
	then max_int
	else 
	  if Bitv.unsafe_get vec p 
	  then p 
	  else gf_rec (succ p) 
      in
      gf_rec pos
	
    let mem i (_, _, vec) = Bitv.get vec i
	
    let to_list (_, _, vec) = Bitv.to_list vec

    let to_seq l = Cf_seq.of_list (to_list l)
	
    let of_list n l =  (* produces a 't with true positions matching list elements *)
      let v = Bitv.create n false in
      let rec ol_rec c f = function (* most efficient if list is in reverse sorted order *)
	  [] -> (Some c, f, v)
	| h :: t when h < f -> Bitv.set v h true; ol_rec (succ c) h t
	| h :: t when h = f -> ol_rec c f t (* duplicated minimum; will ignore values of n at beginning of list *)
	| h :: t when Bitv.get v h -> ol_rec c f t (* not very efficient, but needed to make sure c is correct *) 
	| h :: t when h < n -> Bitv.set v h true; ol_rec (succ c) f t 
	| _ -> assert false (* list element out of range *) in
      ol_rec 0 max_int l

    let xor_list ((c1, f1, v1) as l1) ((c2, f2, v2) as l2) = 
      assert (Bitv.length v1 = Bitv.length v2);
      if c2 = max_int then l1 (*if l2 is empty, return l1*)
      else if c1 = max_int then l2 (* if l1 is empty, return l2 *)
      else 
	let vec = Bitv.bw_xor v1 v2 in
	let first = 
	  if f1 > f2 then f2
	  else if f2 > f1 then f1
	  else (* f1 = f2, and we need to find the new first *)
	    get_first vec f1
	in
	if first = max_int 
	then (Some 0, first, vec) 
	else (None, first, vec)
	    
    let unsafe_remove_element i (c, f, v) = 
      let vec_out = Bitv.copy v in
      Bitv.set vec_out i false;
      let first = if i=f then get_first vec_out f else f in
      match c with
	Some c -> (Some (pred c), first, vec_out)
      | None -> (None, first, vec_out)

    let remove_element i l = 
      if i < Bitv.length l.list then invalid_arg "Nlist2.remove_element: index out of bounds"
	if mem l i then unsafe_remove_element l i 
	else () (*failwith "Nlist2.remove_element: Not_found"*)
	    
    let unsafe_add_element i (c, f, v) = 
      let vec_out = Bitv.copy v in
      Bitv.set vec_out i true;
      let first = min f i in
      match l.count with 
	Some c -> (Some (succ c), first, vec_out)
      | None -> (None, first, vec_out)

    let add_element i l =
      if (i > Bitv.length l.list) then invalid_arg (Printf.sprintf "Nlist2.add_element: len = %d, tried to insert %d" (Bitv.length l.list) i)
      else if mem l i then () else unsafe_add_element l i

    let get_first (f, _, _) = f
  end

let Mutable = 
  struct



  end
