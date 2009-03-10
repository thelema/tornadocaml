type t = {mutable count : int option; mutable first: int; mutable list: Bitv.t}

let new_list n = {count = Some 0; first = max_int; list = Bitv.create n false}

let singleton n i = 
  assert (0 <= i && i < n);
  {count = Some 1; first = i; list = let l = Bitv.create n false in Bitv.set l i true; l}

let scan l =
  let as_list = Bitv.to_list l.list in
  l.count <- Some (List.length as_list);
  let real_first = match as_list with [] -> max_int | x :: _ -> x in
  assert(real_first = l.first)
    
let get_first v pos =   (* returns the first true in vector v not before v.(pos) or max_int if none*)
  let len = Bitv.length v in
  let rec gf_rec p =
    if p >= len then max_int
    else if Bitv.unsafe_get v p then p else gf_rec (succ p) in
  gf_rec pos
    
let mem l i = Bitv.get l.list i
    
let to_list l = Bitv.to_list l.list
    
let of_list n l =  (* produces a 't with true positions matching list elements *)
  let v = Bitv.create n false in
  let rec ol_rec c f = function (* most efficient if list is in reverse sorted order *)
      [] -> {count = Some c; first = f; list = v}
    | h :: t when h < f -> Bitv.set v h true; ol_rec (succ c) h t
    | h :: t when h = f -> ol_rec c f t (* duplicated minimum; will ignore values of n at beginning of list *)
    | h :: t when Bitv.get v h -> ol_rec c f t (* not very efficient, but needed to make sure c is correct *) 
    | h :: t when h < n -> Bitv.set v h true; ol_rec (succ c) f t 
    | _ -> assert false (* list element out of range *) in
  ol_rec 0 max_int l

let xor_list l1 l2 = 
  assert (Bitv.length l1.list = Bitv.length l2.list);
  if l2.first != max_int then (*if l2 is empty, do nothing*)
    if l1.first = max_int then begin (* if l1 is empty, copy l2 to l1 *)
      l1.count <- l2.count; l1.first <- l2.first; l1.list <- Bitv.copy l2.list
    end else begin
      l1.list <- Bitv.bw_xor l1.list l2.list;
      l1.count <- None; (* this is the one case we lose count *)
      if l1.first > l2.first then l1.first <- l2.first
      else if l1.first = l2.first then begin 
	l1.first <- get_first l1.list l1.first;
	if l1.first = max_int then l1.count <- Some 0
      end
    end
	  
let unsafe_remove_element l i = 
  Bitv.set l.list i false;
  if l.first = i then l.first <- get_first l.list i;
  match l.count with 
    Some c -> l.count <- Some (pred c);
  | None -> ()

let remove_element l i = 
  assert (i < Bitv.length l.list);
  if mem l i then unsafe_remove_element l i else ()
      
let unsafe_add_element l i = 
  if l.first > i then l.first <- i;
  Bitv.set l.list i true;
  match l.count with 
    Some c -> l.count <- Some (succ c)
  | None -> ()

let add_element l i =
  if (i > Bitv.length l.list) then raise (Invalid_argument (Printf.sprintf "Nodelist.add_element len = %d, tried to insert %d" (Bitv.length l.list) i))
  else if mem l i then () else unsafe_add_element l i

let size l =
  if l.count = None then scan l;
  match l.count with 
    None -> assert false (*scan failed to set count*)
  | Some c -> c

let get_first l = l.first
