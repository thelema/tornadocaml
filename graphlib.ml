open Discrete;;
open Printf;;

let jout = Cf_journal.stdout
let (==>) x f = f x

(*
type check = int
type data = int
type edge = data * check

type block_id = Check of check | Data of data
let bid_to_string = function Check c -> sprintf "C%d" c | Data d -> sprintf "D%d" d

let dlist_to_blist dlist = List.map (fun x -> Data x) dlist

let clist_to_blist clist = List.map (fun x -> Check x) clist
*)

(* let time str func = Log.time "GraphLib" true str func *)

module type TypeModule = sig
  type t
  val image : t -> string
end

module type BiTyped = sig
  module N : TypeModule
  module P : TypeModule
  type t
end
      
module type Core =
  sig
    include BiTyped
    val nil : t
	
(** [edges t] returns the number of connections in [t] *)
    val edges : t -> int 
(** [mem (n, p) t] is true iff [n] is connected to [p] *)
    val mem : N.t * P.t -> t -> bool 
	
(** [size t] returns the number of nodes in t *)
    val size : t -> int
(** [nodes t] returns a sequence of all nodes *)
    val nodes : t -> N.t Cf_seq.t 
(** [get_conns n t] returns the sequence of points connected to [n] *)
    val get_conns : N.t -> t -> P.t Cf_seq.t 
(** [degree n t] returns the number of points connected to [n] *)	
    val degree : N.t -> t -> int
	
(*
(** [print_view t] prints all the connections in [t] *)
   val print_view : t -> unit
 *)
	
(** [put (n, p) t] returns [t] with [n] connected to [p] *)
    val put : N.t * P.t -> t -> t 
(** [clear (n, p) t] returns [t] with [n] not connected to [p] *)
    val clear : N.t * P.t -> t -> t 
(** [xor_node n1 n2 t] xors the connections of l2 with those of l1 *)
    val xor_node : N.t -> N.t -> t -> t 
(** [clear_node n t] returns t with x connected to nothing *)
    val clear_node : N.t -> t -> t 
  end


module SB1 : Core = 
  struct 
    module YSet = Set.Make(Cf_ordered.Int_order)
    module XMap = Map.Make(Cf_ordered.Int_order)

    module N = struct type t = int let image = string_of_int end
    module P = N
	
    type t = YSet.t XMap.t
    let nil = XMap.empty
	
    let get_conns x t = Cf_seq.of_list (YSet.elements (XMap.find x t))
    let degree x t = YSet.cardinal (XMap.find x t)
	
    let edges t = XMap.fold (fun _ ys acc -> acc + YSet.cardinal ys) t 0
    let mem (x, y) t = try YSet.mem y (XMap.find x t) with Not_found -> false
	
    let size t = XMap.fold (fun _ _ acc -> acc + 1) t 0
    let nodes t = Cf_seq.of_list (XMap.fold (fun x _ -> function [] -> [x] | h :: t when x = h -> h :: t | l -> x :: l) t [])
	
    let put (x,y) t = 
      try 
	let old_set = XMap.find x t in (* Not_found *)
	if YSet.mem y old_set then t
	else XMap.add x (YSet.add y old_set) t
      with Not_found -> (* need to make a new map entry *)
	XMap.add x (YSet.singleton y) t
	  
    let clear (x, y) t =
      try 
	let old_set = XMap.find x t in (* Not_found *)
	if YSet.mem y old_set then
	  let new_set = YSet.remove y old_set in
	  XMap.add x new_set t
	else t
      with Not_found -> t

    let xor_node x1 x2 t =
      try
	let s1 = XMap.find x1 t
	and s2 = XMap.find x2 t in
	let s2' = YSet.union (YSet.diff s1 s2) (YSet.diff s2 s1) in
	XMap.add x2 s2' t
      with Not_found -> t
	  
    let clear_node x t = XMap.remove x t
  end
	

module SB2 : Core = 
  struct 
    module YSet = Cf_rbtree.Set(Cf_ordered.Int_order)
    module XMap = Cf_rbtree.Map(Cf_ordered.Int_order)

    module N = struct type t = int let image = string_of_int end
    module P = N

    type t = YSet.t XMap.t
    let nil = XMap.nil

    let get_conns x t = YSet.to_seq_incr (XMap.search x t)
    let degree x t = YSet.size (XMap.search x t)
	
    let edges t = XMap.fold (fun acc (_, ys) -> acc + (YSet.size ys)) 0 t
    let mem (x, y) t = 
      try YSet.member y (XMap.search x t) 
      with Not_found -> false
	  
    let size t = XMap.fold (fun acc (_, ys) -> if YSet.empty ys then acc else acc + 1) 0 t

	(*TODO: test whether it's faster to eliminate dead indexes in the map
	   when removing or to just filter when returning lists *)
    let nodes t = 
      let accum l (x, s) = match l with
	[] when YSet.empty s -> [] 
      | [] (* otherwise *) -> [x]
      | (h :: t) as l when x = h -> l
      | l -> x :: l 
      in
      Cf_seq.of_list (XMap.fold accum [] t)

    let put (x,y) t = 
      try XMap.modify x (YSet.put y) t
      with Not_found -> 
	XMap.replace (x, (YSet.singleton y)) t
	  
    let clear (x, y) t =
      try XMap.modify x (YSet.clear y) t
      with Not_found -> t

    let xor_node x1 x2 t =
      try 
	let s1 = XMap.search x1 t in
	let replacer s2 = YSet.union (YSet.diff s1 s2) (YSet.diff s2 s1) in
	XMap.modify x2 replacer t
      with Not_found -> t

    let clear_node x t = XMap.delete x t
	
    let print_view t = 
      let print_x x =
	let ynodes = get_conns x t in
	jout#info "# %d: " x;
	Cf_seq.iterate (fun y -> jout#info "%d " y) ynodes;
	jout#info "\n" in
      Cf_seq.iterate print_x (nodes t)

  end  

let countup_inf = Cf_seq.unfold (fun a -> Some (succ a, a)) 0
let countup c = Cf_seq.limit c countup_inf

let filter_to_seq (indexed_check : int -> bool) count =
  Cf_seq.filter indexed_check (countup count)
    
let nonzero_indexes arr = 
  Cf_seq.filter (fun a -> arr.(a) != 0) (countup (Array.length arr))

let replace_array_element idx repl arr = 
  Array.init (Array.length arr) 
    (fun i -> if i = idx then repl arr.(i) else arr.(i))
    

module type CMod = 
  sig
    module N : TypeModule
    module P : TypeModule
    module Uncached : Core with module N = N and module P = P
    module Cached : Core with module N = N and module P = P
    val sync : Uncached.t -> Cached.t
    val unsync : Cached.t -> Uncached.t
  end

module LiftBT (C : Core) : CMod = 
  struct
    module N = C.N
    module P = C.P
    module Uncached = C
    module Cached = C
    let sync t = t
    let unsync t = t
  end

module type Graph = 
  sig
    
(** [L.t] is the type of lefts *)
    module LT : TypeModule
(** [R.t] is the type of rights *)
    module RT : TypeModule
(** [t] is stores connections between lefts and rights *)
    type t
(** [nil] is the empty Graph *)
    val nil : t

    module L : sig
(** [size t] returns the number of nodes in t *)
      val size : t -> int
(** [nodes t] returns a sequence of all nodes *)
      val nodes : t -> LT.t Cf_seq.t 
(** [get_conns n t] returns the sequence of points connected to [n] *)
      val get_conns : LT.t -> t -> RT.t Cf_seq.t 
(** [degree n t] returns the number of points connected to [n] *)	
      val degree : LT.t -> t -> int
(** [print_view t] prints all the connections in [t] *)
      val print_view : t -> unit
(** [clear_node n t] returns t with x connected to nothing *)
      val clear_node : LT.t -> t -> t 
(** [xor_node n1 n2 t] xors the connections of l2 with those of l1 *)
      val xor_node : LT.t -> LT.t -> t -> t 
    end
	
    module R : sig
(** [size t] returns the number of nodes in t *)
      val size : t -> int
(** [nodes t] returns a sequence of all nodes *)
      val nodes : t -> RT.t Cf_seq.t 
(** [get_conns n t] returns the sequence of points connected to [n] *)
      val get_conns : RT.t -> t -> LT.t Cf_seq.t 
(** [degree n t] returns the number of points connected to [n] *)	
      val degree : RT.t -> t -> int
(** [print_view t] prints all the connections in [t] *)
      val print_view : t -> unit
(** [clear_node n t] returns t with x connected to nothing *)
      val clear_node : RT.t -> t -> t 
(** [xor_node n1 n2 t] xors the connections of l2 with those of l1 *)
      val xor_node : RT.t -> RT.t -> t -> t 
    end

	  
(** [edges t] returns the number of connections in [t] *)
    val edges : t -> int 
(** [mem (n, p) t] is true iff [n] is connected to [p] *)
    val mem : LT.t * RT.t -> t -> bool 
(** [put (n, p) t] returns [t] with [n] connected to [p] *)
    val put : LT.t * RT.t -> t -> t 
(** [clear (n, p) t] returns [t] with [n] not connected to [p] *)
    val clear : LT.t * RT.t -> t -> t 

(** [unbalanced t] returns true if L.size > R.size *)
    val unbalanced : t -> bool
(** [filter lf rf t] returns the subgraph containing only those left nodes for which [lf n] is true and similarly for right nodes *)
    val filter : (LT.t -> bool) -> (RT.t -> bool) -> t -> t
(*
    val is_empty : t -> bool
    val dump_edges : t -> (X.t * Y.t) Cf_seq.t
*)
  end

module Graph 
    (CML : CMod) 
    (CMR : CMod with module N = CML.P and module P = CML.N) 
    : Graph =
  struct

    module LT = CML.N
    module RT = CML.P

    module LC = CML.Cached
    module RC = CMR.Cached

    type t = LC.t * RC.t
    
    let nil = (LC.nil, RC.nil)

    let transact usync sync act t = t ==> usync ==> act ==> sync

    module L = 
      struct
	let transact = transact CMR.unsync CMR.sync
	let size (t, _) = LC.size t
	let nodes (t, _) = LC.nodes t
	let get_conns x (t, _) = LC.get_conns x t
	let degree x (t, _) = LC.degree x t
	let print_view (t, _) = 
	  let print_x x =
	    let ynodes = LC.get_conns x t in
	    jout#info "# %s: " (LT.image x);
	    Cf_seq.iterate (fun y -> jout#info "%s " (RT.image y)) ynodes;
	    jout#info "\n" in
	  Cf_seq.iterate print_x (LC.nodes t)
	let clear_node x (tl, tr) = 
	  let yseq = LC.get_conns x tl in
	  let tl' = LC.clear_node x tl
	  and tr' = transact (
	    fun utr -> 
	      let clear acc y = CMR.Uncached.clear (y,x) acc in
	      Cf_seq.fold clear utr yseq
	   ) tr in
	  (tl', tr')
	let xor_node x1 x2 (tl, tr) = 
	  let yseq = LC.get_conns x1 tl in
	  let tl' = LC.xor_node x1 x2 tl
	  and tr' = transact (
	    fun utr -> 
	      let toggle acc y = 
		if CMR.Uncached.mem (y, x2) acc
		then CMR.Uncached.clear (y, x2) acc
		else CMR.Uncached.put (y,x2) acc
	      in
	      Cf_seq.fold toggle utr yseq
	   ) tr in
	  (tl', tr')
	    
      end

    module R = 
      struct
	let transact = transact CML.unsync CML.sync
	let size (_, t) = RC.size t
	let nodes (_, t) = RC.nodes t
	let get_conns x (_, t) = RC.get_conns x t
	let degree x (_, t) = RC.degree x t
	let print_view (_, t) = 
	  let print_x x =
	    let ynodes = RC.get_conns x t in
	    jout#info "# %s: " (RT.image x);
	    Cf_seq.iterate (fun y -> jout#info "%s " (LT.image y)) ynodes;
	    jout#info "\n" in
	  Cf_seq.iterate print_x (RC.nodes t)
	let clear_node x (tl, tr) = 
	  let yseq = RC.get_conns x tr in
	  let tl' = transact (
	    fun utl -> 
	      let clear acc y = CML.Uncached.clear (y, x) acc in
	      Cf_seq.fold clear utl yseq
	   ) tl
	  and tr' = RC.clear_node x tr
	  in
	  (tl', tr')
	let xor_node x1 x2 (tl, tr) = 
	  let yseq = RC.get_conns x1 tr in
	  let tl' = 
	    let tgl t y = 
	      if CML.Uncached.mem (y, x2) t 
	      then CML.Uncached.clear (y, x2) t 
	      else CML.Uncached.put (y, x2) t
	    in
	    transact ( fun utl -> Cf_seq.fold tgl utl yseq ) tl
	  and tr' = RC.xor_node x1 x2 tr
	  in
	  (tl', tr')
      end

    let edges (lt, rt) = 
      let le = LC.edges lt and re = RC.edges rt in
      assert (le = re); le

    let inv (l, r) = (r, l)

    let mem e (lt, rt) = 
      let lm = LC.mem e lt and rm = RC.mem (inv e) rt in
      assert (lm = rm); rm
    let put e (lt, rt) =
      if mem e (lt, rt) 
      then (lt, rt) 
      else (LC.put e lt, RC.put (inv e) rt)
    let clear e (lt, rt) =
      if mem e (lt, rt)
      then (LC.clear e lt, RC.clear (inv e) rt)
      else (lt, rt)

    let unbalanced t = 
      L.size t > R.size t
    let filter lf rf t =
      let lnodes = L.nodes t in
      let ldel = Cf_seq.filter (fun n -> not (lf n)) lnodes in
      let t' = Cf_seq.fold (fun t l -> L.clear_node l t) t ldel in
      let rnodes = R.nodes t' in
      let rdel = Cf_seq.filter (fun n -> not (rf n)) rnodes in
      Cf_seq.fold (fun t r -> R.clear_node r t) t' rdel

end

(*
module OPSet = Set.Make(struct
  type t = X.t * Y.t
  let compare (x1,y1) (x2,y2) = 
    if x1 = x2 
    then Y.compare y1 y2 
    else X.compare x1 x2
end)
    
module SparseBipartiteGraph1 : CoreGraph = 
  struct
    type t = OPSet.t
    let empty = OPSet.empty
    let edges = OPSet.cardinal
	
	let optmap f adder t a0 = 
	  let f' e acc = 
	    match f e with 
	      Some e' -> adder e' acc 
	    | None -> acc in
	  OPSet.fold f' t a0

	let xnodes set = optmap (fun (x, _) -> Some x) XS.put set XS.nil
	let ynodes set = optmap (fun (_, y) -> Some y) YS.put set YS.nil
	    
	let mem e set = OPSet.mem e set
	    
	let get_conns_x x set = 
	  optmap (fun (x', y) -> if x'=x then Some y else None)
	    YS.put set YS.nil
	    
	let get_conns_y y set =
	  optmap (fun (x, y') -> if y'=y then Some x else None)
	    XS.put set XS.nil
	    
	let put e set = OPSet.add e set
	let clear e set = OPSet.remove e set
	    
	let clear_x_node x set = OPSet.filter (fun (x',_) -> x' <> x) set
	let clear_y_node y set = OPSet.filter (fun (_,y') -> y' <> y) set
      end
*)
   
let randlist list =
  let arr = Array.of_list list in
  let len = Array.length arr in
  for i=0 to len - 1 do
    let pos = Random.int len in
    let temp = arr.(pos) in
    arr.(pos) <- arr.(i);
    arr.(i) <- temp
  done;
  Array.to_list arr
    
(** filter to keep k of t items randomly, using the short-straw method.  
    because of state, must not be run more than t times. *)

let short_straw k' t' = 
  let k = ref k' 
  and t = ref t' in
  fun _ -> 
    let prob = (float_of_int !k) /. (float_of_int !t) in
    if Random.float 1. <= prob then (decr k; decr t; true)
    else (decr t; false)


let gen_freq_seq counts =
  let countup = Cf_seq.unfold (fun a -> Some (a, succ a)) 0
  and dupn (i, n) = 
    let dupn' a = if a = 0 then None else Some (i, pred a) in
    Cf_seq.unfold dupn' n
  in
  Cf_seq.seqmap dupn (Cf_seq.combine countup counts)

exception Broken_input of (string)
exception Hit_target
    

      
    
let repeat trials rand_fun = 
  let rec rep' tri tot = 
    if tri = 0 then tot
    else rep' (pred tri) (tot + rand_fun() )
  in
  rep' trials 0

module Decode (G : Graph) =
  struct
    open G

    let short_decode g =
      let dropdata g r = 
	if R.degree r g != 1 then g
	else 
	  let l = Cf_seq.head (R.get_conns r g) in
	  L.clear_node l g
      in
      Cf_seq.fold dropdata g (R.nodes g)

    let gje_simplify g =
      let module RS = Set.Make(struct let compare = Pervasives.compare include RT end) in
      let (fix, is_fixed) =
	let s = ref RS.empty in
	let fix' x = s := RS.add x !s 
	and is_fixed' x = RS.mem x !s in
	(fix', is_fixed') in
      let rec optimize g elem =
	ignore(jout#debug "E%s " (LT.image elem));
	let checks = L.get_conns elem g in
	let (kept,nkept) = Cf_seq.partition is_fixed checks in
	match Lazy.force nkept with
	  Cf_seq.Z -> g
	| Cf_seq.P (piv, nkept') -> 
	    ignore(jout#debug "P%s " (RT.image piv)); fix piv;
	    let xor g r = R.xor_node piv r g in
	    let g' = Cf_seq.fold xor g nkept' in
	    Cf_seq.fold xor g' kept
      in
      Cf_seq.fold optimize g (L.nodes g)

    open Sizeheap
	
    let heap_decode g =
      let heap = create (R.size g+2) in
      let heapadd c = insert heap c (R.degree c g) in
      Cf_seq.iterate heapadd (R.nodes g);
      let gout = ref g in
      let heapdel d g = 
	Cf_seq.iterate (decrement heap) (L.get_conns d g);
	L.clear_node d g
      in
      while look_min_size heap = 1 do
	let conns = R.get_conns (popmin_id heap) g in
	match Lazy.force conns with
	  Cf_seq.P(d, t) when Lazy.force t = Cf_seq.Z ->
	    ignore (jout#debug "D%s " (LT.image d)); 
	    gout := heapdel d !gout
	| _ -> assert false
      done;
      !gout
	
(* TARGETED DECODING ROUTINES *)
    let short_decodet t g =
      let dropdata g r = 
	if R.degree r g != 1 then g
	else 
	  let l = Cf_seq.head (R.get_conns r g) in
	  if l = t then raise Hit_target
	  else L.clear_node l g
      in
      Cf_seq.fold dropdata g (R.nodes g)
	
    let heap_decodet t g =
      let heap = create (R.size g+2) in
      let heapadd c = insert heap c (R.degree c g) in
      Cf_seq.iterate heapadd (R.nodes g);
      let gout = ref g in
      let heapdel d g = 
	if d = t then raise Hit_target;
	Cf_seq.iterate (decrement heap) (L.get_conns d g);
	L.clear_node d g
      in
      while look_min_size heap = 1 do
	let conns = R.get_conns (popmin_id heap) g in
	match Lazy.force conns with
	  Cf_seq.P(d, t) when Lazy.force t = Cf_seq.Z ->
	    ignore (jout#debug "D%s " (LT.image d)); 
	    gout := heapdel d !gout
	| _ -> assert false
      done;
      !gout
	
    let decodable g =
      let g' = gje_simplify (short_decode (short_decode g)) in
      not (unbalanced g')
(*      jout#info "#  pre:"; graph#print_checks;*)
(*      jout#info "#  mid:"; graph#print_checks;*)
(*      jout#info "# mid2:"; graph#print_checks;*)
(*    self#hdecode; *)
(*      jout#info "# post:"; graph#print_checks;*)
	
(*
   let candecodet g = function 
   Data t -> 
   try 
   let g' = gje_simplify (heap_decodet t (short_decodet t g)) in
   not (unbalanced g')
   with Hit_target -> true
   | Check _ -> 
   let g' = gje_simplify (heap_decode (short_decode g)) in
   not (unbalanced g')
 *)   
	
(* GRAPH CREATION FOR TESTING GRAPHS *)
    let generatepair pairs_list = 
      let fput g e = G.put e g in
      List.fold_left fput G.nil pairs_list 
    let newdata dl g = 
      G.filter (fun d -> List.mem d dl) (fun _ -> true) g
    let newtest dataneed checkmissing g = 
      G.filter (fun d -> List.mem d dataneed) (fun c -> not (List.mem c checkmissing)) g
    let randsub numdata numcheck g =
      let dfilt = short_straw numdata (L.size g)
      and cfilt = short_straw (R.size g - numcheck) (R.size g) in
      G.filter dfilt cfilt g
(*
   method newblocks_gone blocks =
   let rec split dlist clist blist = match blist with 
   [] -> (dlist, clist)
   | Data d :: rest -> split (d :: dlist) clist rest
   | Check c :: rest -> split dlist (c :: clist) rest in
   let (dataneed, checkneed) = split [] [] blocks in
   let checkmissing = List.filter (fun x -> not (List.mem x checkneed)) self#cnodes in (*Make More Efficient*)
   self#newtest dataneed checkmissing

    let ranksub graph erasures =
      let subgraph = graph#randsub erasures 0 in
      decodable (randsub fulldecode subgraph
*)
	
  end
    
    
(*
(*  DECODING ROUTINES                         *)
   
   
   
(* DECODING HELPER FUNCTIONS *)
   let testrank graph trials erasures = 
   repeat trials 
   (fun () -> if ranksub graph erasures then 1 else 0)
   let rankreal graph erasures = 
   let keepdata = ref graph#dcount in
   let keepcheck = ref graph#ccount in
   if !keepdata + !keepcheck < erasures then 
   raise (Broken_input 
   (sprintf "rankreal: %d, %d, %d" !keepdata !keepcheck erasures ));
   for i = 1 to erasures do 
   if Random.int (!keepdata + !keepcheck) < !keepdata 
   then keepdata := !keepdata - 1
   else keepcheck := !keepcheck - 1
   done;
   let numdata = graph#dcount - !keepdata in
   let numcheck = graph#ccount - !keepcheck in
   assert (numdata + numcheck = erasures);
   let subgraph = graph#randsub numdata numcheck in
   fulldecode subgraph
   let testreal graph trials erasures =
   try 
   repeat trials 
   (fun () -> if rankreal graph erasures then 1 else 0)
   with Broken_input x -> 
   eprintf "Testreal (%d)\n" erasures;
   eprintf "self: ";  graph#print_checks;
   raise (Broken_input x)
   let rec stress graph blocks_gone block_left = 
   match block_left with 
   deletion :: remaining -> 
   let new_gone = deletion :: blocks_gone in 
   let test_graph = graph #newblocks_gone new_gone in
   if test_graph#candecodet deletion
   then stress graph new_gone remaining
   else new_gone
   | [] -> raise (Broken_input "Ran out of blocks to add")
   let stress_data graph trials = 
   repeat trials 
   (fun () -> List.length (stress graph [] ((randlist graph#dblocks) @ graph#cblocks)))
   let stress_real graph trials = 
   repeat trials
   (fun () -> List.length (stress graph [] (randlist (graph#dblocks @ graph#cblocks))))


   
   end;;
 *)


(*  GRAPH GENERATION                          *)
(*
   let rec gen_graph default bounds gentype = 
   match (bounds, gentype) with 
   | (_,`S1) -> SparseBipartiteGraph1.empty
   | (_,`S2) -> SparseBipartiteGraph2.empty
   | (_,`S3) -> SparseBipartiteGraph2.empty
   | (Some (d, c), `D1) -> new denseBipartiteGraph1 d c
   | (Some (d, c), `D2) -> new denseBipartiteGraph2 d c
   | (Some (d, c), `D3) -> new denseBipartiteGraph3 d c
   | (b, _) -> gen_graph `S3 b default
   
(* Graph generation from various lists *)
   let generate_rand data check outtype =
   let (dlist, dcount) = gen_freq_list data
   and (clist, ccount) = gen_freq_list check in
   let dlist = randlist(dlist) in
   let outgraph = gen_graph `S3 (Some dcount, ccount) outtype in
   List.iter2 outgraph#add llist rlist;
   outgraph
   let generateedg data check outtype =
   let outgraph = gen_graph `S3 None outtype in
   List.iter2 outgraph#add data check;
   outgraph
   let generatepair pair_list outtype =
   let outgraph = gen_graph `S3 None outtype in
   List.iter (fun (d,c) -> outgraph#add d c) pair_list;
   outgraph

(* Graph generation based on parameterized known functions *)
   let generate_regular (num_edges: int) (num_data: int) outtype =
   let bounds = Some (num_data, num_edges / num_data) in    
   let outgraph = gen_graph B1 bounds outtype in
   for i = 0 to num_edges-1 do 
   outgraph#add (i mod num_data) (i / num_data) 
   done;
   outgraph
   let generate_MDS (n: int) (irr: int) (checks: int) outtype =
   let bounds = Some ((1 lsl n), checks) in
   let outgraph = gen_graph B1 bounds outtype in
   let field = fun () -> new Bffield.bffield n irr in
   let array = fun () -> field#mds_array (checks/n) in
   let rows = Array.length array and cols = Array.length array.(0) in
   let getbit int pos = ((int lsr pos) land 1) in
   for c=0 to (rows*n)-1 do
   let fromrow = c / n and frombit = c mod n in
   for d = 0 to cols-1 do
   let elem = field#out array.(fromrow).(d) in
   if getbit elem frombit = 1 then
   outgraph#add d c;
   done;
   done;
   outgraph

   let generate_cid (data: int) (checks: int) outtype =
   let outgraph = gen_graph B1 (Some (data, checks)) outtype in
(*    let pattern_size = data / checks in (* FIXME: figure out what's up with this variable *) *)
   for datanum = 0 to data-1 do
   let patnum = datanum / checks in
   let shiftnum = datanum mod checks in
   outgraph#add datanum shiftnum;
   outgraph#add datanum ((shiftnum + patnum) mod checks);
   done;
   outgraph
 *)

(** Takes two complementary BpParts and joins them to have efficient x
   and y functions, instead of just x functions *)
(*
   module JoinedGraph 
   (XG':PartialGraph) (YG':PartialGraph) 
   (X: DiscreteRange) (Y: DiscreteRange) =
   struct
   module XG = XG'(X)(Y)
   module YG = YG'(Y)(X)
   module XS = Set.Make(X)
   module YS = Set.Make(Y)

   type t = int * XG.t * YG.t
   let empty = (0, XG.empty, YG.empty)
   
   let edges (e, _, _) = e
   let xnodes (_, xm, _) = XG.xnodes xm
   let ynodes (_, _, ym) = YG.xnodes ym
   let get_conns_x x (_, xm, _) = XG.get_conns_x x xm
   let get_conns_y y (_, _, ym) = YG.get_conns_x y ym
   let mem x y (_, xm, _) = XG.mem x y xm
   let add x y (e, xm, ym) = 
   let (fail, xm') = XG.add x y xm in 
   if fail then
   (true, (e, xm, ym))
   else
   let _, ym' = YG.add y x ym in
   (false, (e+1, xm', ym'))
   let set x y t = snd (add x y t)
   let extract x y (e, xm, ym) = 
   let (nofail, xm') = XG.extract x y xm in
   if nofail then 
   let (_, ym') = YG.extract y x ym in
   (true, (e-1, xm', ym'))
   else
   (false, (e, xm, ym))
   let clear x y t = snd (extract x y t)
   let clear_x_node x (e, xm, ym) = 
   let ylist = XG.get_conns_x x xm in
   let xm' = XG.clear_x_node x xm
   and ym' = YS.fold (fun y ym -> snd (YG.extract y x ym)) ylist ym 
   in
   (e-(YS.cardinal ylist), xm', ym')
   let clear_y_node y (e, xm, ym) = 
   let xlist = YG.get_conns_x y ym in
   let xm' = XS.fold (fun x xm -> snd (XG.extract x y xm)) xlist xm
   and ym' = YG.clear_x_node y ym
   in
   (e-(XS.cardinal xlist), xm', ym')
   end


   module type FullGraph = sig
   include CoreGraph
(*
   val generatepair : (X.t * Y.t) Cf_seq.t -> t
   val filter_data : XS.t -> t -> t
   val filter_test : XS.t -> YS.t -> t -> t
   val rand_sub : int -> int -> t -> t
 *)

(*
   val dblocks : t -> block_id list
   val cblocks : t -> block_id list
   val all_blocks : t -> block_id list
   val filter_blocks : block_id list -> t -> t
 *)
   end
   
   module Complete_core (C : CoreGraph) : FullGraph = struct
   include C
   let is_empty t = edges t = 0
(*
   let data_min t = List.fold_left min (xnodes t)
   let data_max t = List.fold_left max (xnodes t) 
   let check_min t = List.fold_left min (ynodes t) 
   let check_max t = List.fold_left max (ynodes t) 
 *)
   let xcount t = XS.size (xnodes t)
   let ycount t = YS.size (ynodes t)
   let unbalanced t = xcount t > ycount t
   let xcount_of x t = YS.size (get_conns_x x t)
   let ycount_of y t = XS.size (get_conns_y y t)
   let dump_uedges t = 
   let xn = XS.to_seq_incr (xnodes t)
   and gather x = 
   let makepair y = (x,y) in
   let get_ys = YS.to_seq_incr (get_conns_x x t) in
   Cf_seq.map makepair get_ys
   in
   Cf_seq.seqmap gather xn
   let dump_edges t = dump_uedges t
   let generatepair dcl = Cf_seq.fold (fun g e -> put e g) empty dcl
   let filter_data xs t = XS.fold (fun g x -> clear_x_node x g) t xs
   let filter_both xs ys t = 
   let t' = XS.fold (fun g x -> clear_x_node x g) t xs in
   YS.fold (fun g y -> clear_y_node y g) t' ys
   let filter_test xs ys t = 
   let dely = YS.diff (ynodes t) ys in
   filter_both xs dely t
   let rand_sub xc yc t =
   let rsub filter set to_keep total = 
   filter (short_straw to_keep total) set
   in
   let xs' = xnodes t 
   and ys' = ynodes t in
   let xs = rsub XS.filter xs' xc (XS.size xs')
   and ys = rsub YS.filter ys' yc (YS.size ys') in
   filter_both xs ys t
   let print_y_view t = 
   let print_y y =
   let xnodes = get_conns_y y t in
   jout#info "# %d: " (Y.pos y);
   XS.iterate (fun x -> jout#info "%d " (X.pos x)) xnodes;
   jout#info "\n" in
   YS.iterate print_y (ynodes t)
   let equal t1 t2 = false
   end
(*
(* given element a, a list, and a count, prepends a to the list count times *)
   let rec duplist (a : 'a) (acc : 'a list) n = 
   if n = 0 then acc 
   else duplist a (a::acc) (pred n)

(* generates a list with element frequency given in counts *)

   let gen_freq_list counts = 
   let rec gen_freq_list' acc i (* counts *)= function 
   [] -> (acc, i)
   | n :: rest -> gen_freq_list' (duplist i acc n) (i+1) rest
   in 
   gen_freq_list' [] 0 counts
 *)
 *)

module DiscreteGraph (N : DiscreteRange) (P : DiscreteRange) = 
  struct
    
    module type DCore = Core with module N = N and module P = P
	
    module OrderedPair =
      struct
	type t = N.t * P.t
	let compare (x1,y1) (x2,y2) = 
	  if x1 = x2 
	  then P.compare y1 y2 
	  else N.compare x1 x2
      end
	
    module BB1 : DCore = 
      struct 
	module N = N
	module P = P
	    
	type t = Nlist2.t array
	let nil = Array.create N.width (Nlist2.new_list P.width)
	    
	let edges t = 
	  let add_size acc nl = acc + Nlist2.size nl in
	  Array.fold_left add_size 0 t
	    
	let nodes t = Cf_seq.map N.value (filter_to_seq (fun i -> Nlist2.size t.(i) != 0) N.width)
	let size t = Cf_seq.length (filter_to_seq (fun i -> Nlist2.size t.(i) != 0) N.width)
	    
	let get_conns x t = Cf_seq.map P.value (Nlist2.to_seq t.(N.pos x))
	let degree x t = Nlist2.size t.(N.pos x)
	    
	let mem (x, y) t = Nlist2.mem (P.pos y) t.(N.pos x)
	let put (x, y) t = 
	  replace_array_element (N.pos x) (Nlist2.add_element (P.pos y)) t
	let clear (x, y) t =
	  replace_array_element (N.pos x) (Nlist2.remove_element (P.pos y)) t
	let clear_node x t =
	  replace_array_element (N.pos x) (fun _ -> Nlist2.new_list P.width) t
	let xor_node x1 x2 t =
	  replace_array_element (N.pos x2) (Nlist2.xor_list t.(N.pos x1)) t
	    
      end
	    
    module type DCMod = 
	CMod with module N = N and module P = P

    module Wrap (DC : DCore) : DCMod =
      struct
	module N = N
	module P = P
	module Uncached = DC
	module Cached = DC
	let sync t = t
	let unsync t = t
      end

    module AddColumns : functor (DCM : DCMod) -> DCMod = 
      functor (DCM : DCMod) ->
      struct
	module N = DCM.N
	module P = DCM.P
	module Uncached = DCM.Uncached
	module Cached = 
	  struct
	    module C = DCM.Cached
	    module N = N
	    module P = P
	    type t = C.t * int array
	    let nil = (C.nil, Array.create N.width 0)
	    let nodes (_, c) = Cf_seq.map N.value (nonzero_indexes c)
	    let size (_, c) = Cf_seq.length (nonzero_indexes c)
	    let edges (_, c) = Array.fold_left (+) 0 c
	    let degree x (_, c) = c.(N.pos x) 
	    let get_conns x (b, _) = C.get_conns x b
	    let mem e (b, _) = C.mem e b
	    let put ((x, _) as e) (b, c) = 
	      let c' = replace_array_element (N.pos x) succ c in
	      (C.put e b, c')
	    let clear ((x, _) as e) (b, c) = 
	      let c' = replace_array_element (N.pos x) pred c in
	      (C.clear e b, c')
	    let clear_node x (b, c) =
	      if c.(N.pos x) = 0 then (b,c) else
	      let c' = replace_array_element (N.pos x) (fun _ -> 0) c in
	      (C.clear_node x b, c')
	    let xor_node x1 x2 (b, c) = 
	      let b' = C.xor_node x1 x2 b in
	      let counter _ = C.degree x2 b' in
	      let c' = replace_array_element (N.pos x2) counter c in
	      (b', c')
	  end
	let sync b = 
	  let deg n = DCM.Uncached.degree (N.value n) b in
	  (DCM.sync b, Array.init N.width deg)
	let unsync (b, c) = DCM.unsync b
      end

    module BB2: DCore = 
      struct 
	module N = N
	module P = P
	type t = bool array array
	let nil = Array.create_matrix N.width P.width false 
            (* matrix of whether x is connected to y *)
	let nodes t = 
	  let nonzero i = 
	    let ti = t.(i) in
	    let rec test j = if j < 0 then false else if ti.(j) then true else test (pred j) in
	    test P.width
	  in
	  Cf_seq.map N.value (filter_to_seq nonzero N.width)
	let size t = Cf_seq.length (nodes t) (* TODO: efficiency increase *)
	let degree x t = 
	  let ti = t.(N.pos x) in
	  let rec cnt j acc = if j < 0 then acc else if ti.(j) then cnt (pred j) (succ acc) else cnt (pred j) acc in
	  cnt P.width 0
	let edges t = Cf_seq.fold (+) 0 (Cf_seq.map (fun x -> degree (N.value x) t) (countup N.width))
	let get_conns x m = 
	  let xa = m.(N.pos x) in
	  Cf_seq.map P.value 
	    (filter_to_seq (fun a -> xa.(a)) N.width)
	let mem (x, y) m = m.(N.pos x).(P.pos y)
	let put (x, y) m = 
	  replace_array_element (N.pos x) 
	    (replace_array_element (P.pos y) (fun _ -> true)) m
	let clear (x, y) m =
	  replace_array_element (N.pos x) 
	    (replace_array_element (P.pos y) (fun _ -> false)) m
	let clear_node x m =
	  replace_array_element (N.pos x) (fun _ -> Array.create P.width false) m
	let xor_node x1 x2 m = 
	  let r1 = m.(N.pos x1) in
	  let repl r2 = Array.init P.width (fun i -> r1.(i) != r2.(i)) in
	  replace_array_element (N.pos x2) repl m	
	let print_view t =
	  let print_x x =
	    let ynodes = get_conns x t in
	    jout#info "# %d: " (N.pos x);
	    Cf_seq.iterate (fun y -> jout#info "%d " (P.pos y)) ynodes;
	    jout#info "\n" in
	  Cf_seq.iterate print_x (nodes t)
      end	

    module BB2opt = AddColumns(Wrap(BB2))

    module BB3: DCore = 
      struct 
	module N = N
	module P = P

	let pos x y = x * P.width + y
	let pos' x y = pos (N.pos x) (P.pos y)
	let slice x t = Bitv.sub t (x * P.width) N.width
	let slice' x t = slice (N.pos x) t
	type t = Bitv.t
	let nil = Bitv.create (N.width * P.width) false
                   (* matrix of whether x is connected to y *)
		  

	let edges m = List.length (Bitv.to_list m)
	let nodes m = 
	  let empty x = Bitv.all_zeros (slice x m) in
	  Cf_seq.map N.value (filter_to_seq empty N.width)
	let size m = (* intended to be overridden by AddColumns *)
	  Cf_seq.length (nodes m) 
	let mem (x, y) m = Bitv.get m (pos' x y)
	let get_conns x m = Cf_seq.map P.value 
	    (filter_to_seq (fun a -> Bitv.get m (pos (N.pos x) a)) N.width)
	let degree x m = List.length (Bitv.to_list (slice (N.pos x) m))
	let put (x, y) m = 
	  let m' = Bitv.copy m in 
	  Bitv.set m' (pos' x y) true;
	  m'
	let clear (x, y) m =
	  let m' = Bitv.copy m in 
	  Bitv.set m' (pos' x y) false;
	  m'
	let clear_node x m =
	  let m' = Bitv.copy m in
	  Bitv.fill m' (pos (N.pos x) 0) P.width false;
	  m'
	let xor_node x1 x2 m =
	  let sub1 = slice' x1 m and sub2 = slice' x2 m in
	  let xored = Bitv.bw_xor sub1 sub2 
	  and m' = Bitv.copy m in
	  Bitv.blit xored 0 m' ((N.pos x2)*N.width) N.width;
	  m'
      end

    module BB3opt = AddColumns(Wrap(BB3))
  end
