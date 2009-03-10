open Printf

let log = new Log.logger "Incremental: " 2
let blog = new Log.logger "" 2
  
module type Block = 
  sig
    type data
    val nul_block : data
    val xor: data -> data -> unit (* modifies first argument *)
  end
      
exception InefficientOperation of int * int list

let print_ilist l = let buf = Buffer.create 10 in List.iter (fun s -> Buffer.add_string buf (sprintf "%d " s)) l; Buffer.contents buf
let keys h = Hashtbl.fold (fun k _ acc -> (k::acc)) h []
let print_links links = "(" ^ print_ilist (Nodelist.to_list links) ^ "), "

module Incremental_decoder = 
  functor (B: Block) -> 
  struct
    type partial = {data : B.data; links : Nodelist.t}
    let xor_checks p1 p2 = B.xor p1.data p2.data; Nodelist.xor_list p1.links p2.links
    let xor_data p id data = B.xor p.data data; Nodelist.remove_element p.links id
    let to_block partial = assert (Nodelist.size partial.links = 1); (Nodelist.get_first partial.links, partial.data)
    class virtual decoder_stats = object (self : 'a) 
      val data = [| 0; 0; 0; 0 |]
      val check = ref 0
      val rdata = [| 0; 0; 0; 0 |]
      val rcheck = ref 0
      method virtual get : int -> B.data option 
      method virtual have : int -> bool
      method virtual is_done : bool
      method virtual max_count : int
      method print_stats = log#lnormal (lazy (sprintf "in -> d: %d %d %d %d c: %d  waste: d: %d %d %d %d c: %d  max: %d\n" 
						data.(0) data.(1) data.(2) data.(3) !check rdata.(0) rdata.(1) rdata.(2) rdata.(3) !rcheck self#max_count));
      method din = function i when i < 3 -> data.(i) <- data.(i) + 1 | _ -> data.(3) <- data.(3) + 1
      method rin = function i when i < 3 -> rdata.(i) <- rdata.(i) + 1; self#din i | j -> rdata.(3) <- rdata.(3) + 1; self#din j
      method cin = incr check
      method rcin = incr rcheck
      method add id data = match id with Graphlib.Data i -> self#add_data (i,data) | Graphlib.Check i -> self#add_check (i,data)
      method virtual add_data : int * B.data -> unit
      method virtual add_check : int * B.data -> unit
    end
    class graph_decoder (g: Graphlib.graph) = 
      let last_data = g#data_max + 1 in (* the biggest data index *)
      let to_partial (id, data) = {data = data; links = Nodelist.singleton last_data id} in
      object (self)
	inherit decoder_stats
	val to_go = ref g#dblocks
	val datas = Array.create last_data B.nul_block (* array of known data blocks *)
	val pool = Hashtbl.create (last_data/2) (* partially recovered data from check blocks *)
	method get id = if datas.(id) == B.nul_block then None else Some datas.(id)
	method have id = datas.(id) != B.nul_block
	method is_done = List.length !to_go = 0
	method dump_status ?(lev=0) () = 
	  log#log lev "Have chunks: "; 
	  Array.iteri (fun i d -> if d != B.nul_block then blog#log1 lev "%d " i) datas;
	  blog#log lev "\nTo_go chunks:"; List.iter (fun b -> blog#log lev (Graphlib.bid_to_string b)) !to_go;
	  blog#log lev "\n";
	  blog#llog lev (lazy self#show_pool);
	method add_data block = self#add_data' 0 block; blog#lminor (lazy self#show_pool); blog#minor "\n"
	method add_data' lev = function
	    (id, _) when self#have id -> (* we already have the block *)
	      blog#log1 3 "^R%d^" id; self#rin lev
	  | (id, cont) -> (* new block id=id, contents = cont *)
	      datas.(id) <- cont; self#din lev;
	      to_go := List.filter (fun x -> x <> Graphlib.Data id) !to_go;
	      blog#log1 3 "D%d->" id;
	      try 
		match self#cadd_data id cont with
		  [] -> blog#log 3 "* "
		| recovered -> 
		    blog#log 3 "[";
		    List.iter (self#add_data' (lev+1)) recovered;
		    blog#log 3 "]";
	      with InefficientOperation (i,x) -> log#linfo (lazy((sprintf "Couldn't merge with chunk %d\n" i) ^ print_ilist x ^ "\n")); self#dump_status ~lev:2 (); assert false
	method add_check block = self#add_check' 0 block; blog#lminor (lazy self#show_pool); blog#minor "\n"
	method add_check' lev (id, cont) = 
	  blog#minor1 "C%d->" id; self#cin;
	  let sources = (g#dnodes_of id) in
	  let partial = {data = cont; links = Nodelist.of_list last_data sources} in	  
	  let (already_have, need) = List.partition self#have sources in
	  if need = [] then self#rcin
	  else 
	    let datas_have = List.map (fun id -> datas.(id)) already_have in
	    List.iter (fun d -> B.xor cont d) datas_have; (* xor known data blocks into contents *)
	    match self#cadd_partial {data = cont; links = Nodelist.of_list last_data need} with 
	      [] -> blog#minor "*";
	    | [recovered_data_block] -> 
		blog#minor "[";
		self#add_data' (lev+1) recovered_data_block;
		blog#minor "]";
	    | _ -> assert false (* shouldn't be able to directly decode more than one data block from a single check block *)
		  
	val mutable max_count = 0;
	method max_count = max_count
	method is_empty = Hashtbl.length pool = 0
	method show_pool = 
	  if Hashtbl.length pool = 0 then "Pool Empty" 
	  else begin
	    let buf = Buffer.create 20 in
	    Hashtbl.iter (fun target check -> Buffer.add_string buf (print_links check.links)) pool;
	    Buffer.contents buf
	  end
	method cadd_data id in_data =
	  let pre_rec = (* before recursion recovered blocks *)
	    if Hashtbl.mem pool id then begin 
	      let target = Hashtbl.find pool id in (* raises Not_found *)
	      Hashtbl.remove pool id; 
	      xor_data target id in_data;
	      self#cadd_partial target
	    end else [] in
	  let simplify target partial ((rem, recov) as acc) =
	    if Nodelist.mem partial.links id then
	      begin
		blog#minor1 "S%d " target;
		xor_data partial id in_data;
		match Nodelist.size partial.links with
		  0 -> (target::rem, recov)
		| 1 -> (target::rem, (to_block partial) :: recov)
		| _ -> acc
	      end else acc
	  in 
	  let (remove, recovered) = Hashtbl.fold simplify pool ([], pre_rec) in
	  List.iter (Hashtbl.remove pool) remove;
	  recovered  (* return recovered blocks *)
	method cadd_partial partial =
	  match Nodelist.size partial.links with
	    0 -> [] (* useless partial; no links *)
	  | 1 -> [to_block partial] (* has one link; decode *)
	  | n -> 
	      let target = Nodelist.get_first partial.links in
	      try 
		let colliding_check = Hashtbl.find pool target in (* throws Not_found *)
		xor_checks partial colliding_check;
		self#cadd_partial partial
	      with Not_found -> 
		Hashtbl.add pool target partial;
		blog#minor1 "T%d " target;
		max_count <- max max_count (Hashtbl.length pool);
		[]
      end

    class want_decoder (g: Graphlib.graph) =
      object (self)
	inherit graph_decoder g as gd
	val want_graph = g#clone
	method wanted =
	  match want_graph#cblocks with 
	    [] -> None
	  | c -> let b = List.nth c (Random.int (List.length c)) in
	    log#debug ("W="^(Graphlib.bid_to_string b)); 
	    Some b
	method add_check2 lev (id, cont) = want_graph#delcheck id; gd#add_check' lev (id, cont)
	method add_data2 lev (id, cont) = want_graph#deldata id; gd#add_data' lev (id, cont)
      end

  end;;

