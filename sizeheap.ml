type 'a pair = {id: 'a option; mutable size: int}
let nil = {id = None; size = max_int}
    
type 'a t = {
    mutable last: int; 
    arr : 'a pair array; 
    idx : ('a, int) Hashtbl.t
  }
      
let create size = {
  last = 0;
  arr = Array.create (size + 1) nil; (* don't use index 0 *)
  idx = Hashtbl.create size;
}

let index t idopt pos =
  match idopt with
    None -> ()
  | Some id -> Hashtbl.replace t.idx id pos

let deindex t idopt =
  match idopt with
    None -> failwith "cannot deindex a None id"
  | Some id -> Hashtbl.remove t.idx id

(**
 *  @i : initial pos
 *  @f : final pos
 *  @t : heap
 *)
let move t i f = 
  try
    index t t.arr.(i).id f;
    t.arr.(f) <- t.arr.(i)
  with _ -> Printf.printf "failed moving from %d to %d\n" i f
      
let look_min_id t = t.arr.(1).id
let look_min_size t = t.arr.(1).size (* returns max_int for empty heap *)
let sizeof t id = t.arr.(Hashtbl.find t.idx id).size
    
let percolate_down t startpos = (* moves elem at startpos down *)
  let elem = t.arr.(startpos)
  and lastval = t.arr.(t.last).size 
  and value_at i = 
    try t.arr.(i).size (* nil.size = max_int *)
    with _ -> max_int (* out of bounds -> max_int *)
  in
  let rec loop pos = 
    let lpos = pos * 2 and rpos = pos * 2 + 1 in
    let lsize = value_at lpos and rsize = value_at rpos in
    if elem.size < lsize && elem.size < rsize then begin
      t.arr.(pos) <- elem;
      index t elem.id pos;
    end else 
      let smaller_pos = if lsize < rsize then lpos else rpos in
      let next = 
	if smaller_pos >= t.last || lastval <= value_at smaller_pos 
        then t.last else smaller_pos 
      in
      move t pos next;
      loop next
  in
  loop startpos

let percolate_up t startpos pair =
  let rec loop pos = 
    let next = pos / 2 in
    if t.arr.(next).size <= pair.size || pos <= 1
    then begin
      index t pair.id pos;
      t.arr.(pos) <- pair;
    end else begin 
      move t next pos;
      loop next
    end
  in
  loop startpos
    
let inconsistent t = 
  for i=1 to t.last do 
    match t.arr.(i).id with
      None -> assert (not true)
    | Some id -> assert (Hashtbl.find t.idx id = i);
  done
    
let popmin t = 
  if t.last == 0 
  then raise Not_found (*can't pop an empty heap*)
  else
    let out = t.arr.(1) in
    deindex t out.id;
    percolate_down t 1;
    t.last <- t.last - 1;
    out
let popmin_id t = match (popmin t).id with None -> assert false | Some id -> id

let insert t id value = 
  let record = {id = Some id; size = value} in
  t.last <- t.last + 1;
  try
    percolate_up t t.last record
  with _ -> 
    Printf.printf "failed to insert record with value %d\n" value
      
let remove t id = 
  let pos = Hashtbl.find t.idx id in
  deindex t (Some id);
  percolate_down t pos;
  t.last <- t.last - 1
      
let resize t id size =
  if size <= 0 then remove t id
  else 
    let pos = Hashtbl.find t.idx id in
    let elem = t.arr.(pos) in
    let oldsize = elem.size in
    elem.size <- size;
    if oldsize < size 
    then percolate_down t pos
    else percolate_up t pos elem
let decrement t id =
  let size = sizeof t id in
  resize t id (pred size)
