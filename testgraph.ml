(* Random.self_init;; *)
open Printf;;
open Arg2;;
open Str;;

let log = new Log.logger "TestGraph: " 2
let time str func = Log.time "TestGraph" true str func

exception Unknown_Type
exception Unknown_Form

let push l v = l := v :: !l
let pop l = match !l with [] -> raise (Invalid_argument "pop empty") | h :: t -> l := t; h

(*type 

let parse_range s = 
  match full_split (regexp "-") s with
    [Delim _; Text i] -> (Def, int_of_string i)
  | [Text i; Delim _] -> (int_of_string i, Def)
  | [Text i; Delim _; Text j] ->
      (int_of_string i, int_of_string j)
  | _ -> invalid_arg s
	*)
type graph_type = 
    MDS of int * int * int
  | CID of int * int
  | REG of int * int
type mode = 
    DecodeProb 
  | WorkNeeded 
  | Freenet of float * int (* chance of retrieving a block * # of blocks to queue *)
let act = ref DecodeProb
let form = ref (MDS (8,29,120))
let impl = ref Graphlib.Default;;
let trials = ref 20;;
let int1 = ref 0;;  (* for holding values of arguments to -m and -c *)
let int2 = ref 0;;
let int3 = ref 0;;
let flt1 = ref 0.0;;

let args =
  [ (Both ('1', "S1"), [Unit (fun () -> impl := Graphlib.S1)], [], 
     "Use SparseGraph type 1");
    (Both ('2', "S2"), [Unit (fun () -> impl := Graphlib.S2)], [], 
     "Use SparseGraph type 2");
    (Both ('3', "S3"), [Unit (fun () -> impl := Graphlib.S3)], [], 
     "Use SparseGraph type 3");
    (Both ('4', "S4"), [Unit (fun () -> impl := Graphlib.S4)], [], 
     "Use SparseGraph type 4");
    (Both ('5', "B1"), [Unit (fun () -> impl := Graphlib.B1)], [],
     "Use BoundedGraph type 1");
    (Both ('6', "M1"), [Unit (fun () -> impl := Graphlib.M1)], [], 
     "Use MatrixGraph type 1");
    (Both ('7', "M2"), [Unit (fun () -> impl := Graphlib.M2)], [], 
     "Use MatrixGraph type 2");
    (Both ('t', "trials"), [Int_var trials], [], 
     "Set number of trials to do");
    (Both ('d', "decode"), [Unit (fun () -> act := DecodeProb)], [],
     "Measure decoding probabilities at different erasure levels");
    (Both ('w', "work"), [], 
     [(*String (fun s -> let (n, x) = parse_range s in min := n; max := x), (fun () -> (min,max) := (Def, Def));*)
      Float (fun _ -> act := WorkNeeded), (fun () -> ())],
     "Measure the number of blocks needed to decode");
    (Both_extra ('f', "freenet", "[prob] [queue]"), [],
     [
      (Float_var flt1, (fun () -> flt1 := 0.8));
      (Int_var int1, (fun () -> int1 := 5));
      (Float (fun _ -> act := Freenet (!flt1, !int1)), 
       (fun () -> act := Freenet(!flt1, !int1)))], 
     "Measure the number of blocks that'd have to be requested to decode");
    (Both_extra ('m', "mds", "[logdata] [irr] [checks]"), [],
     [
      (Int_var int1, (fun () -> int1 := 8));
      (Int_var int2, (fun () -> int2 := 29));
      (Int_var int3, (fun () -> int3 := 120));
      (Float (fun _ -> form := MDS (!int1, !int2, !int3)),
       (fun () -> form := MDS (!int1, !int2, !int3)))],
     "Set graph type to mds");
    (Both_extra ('r', "reg", "[edges] [data]"), [],
     [
      (Int_var int1, (fun () -> int1 := 50));
      (Int_var int2, (fun () -> int2 := 25));
      (Float (fun _ -> form := REG (!int1, !int2)),
       (fun () -> form := REG (!int1, !int2)))],
     "Set graph type to regular");
    (Both_extra ('c', "cid", "[data] [checks]"), [],
     [
      (Int_var int1, (fun () -> int1 := 31));
      (Int_var int2, (fun () -> int2 := 7));
      (Float (fun _ -> form := CID (!int1, !int2)),
       (fun () -> form := CID (!int1, !int2)))],
     "Set graph type to cid")]
and usage_info = "testgraph -[1|2|3] [-m|-r|-c] [parameters]"
and descr = "Test various graph implementations"
and notes = "by Eric Norige" in
Arg2.parse args (fun _ -> ()) usage_info descr notes;;


(*module Intblock = 
  struct
    type data = Data of int | Empty
    let nul_block = Empty
    and xor = function Empty -> (function _ -> Empty) | Data x -> (function Empty -> Empty | Data y -> Data (x lxor y))
  end;;
module IntDecoder = Incremental.Incremental_decoder(Intblock);; *)
module NullBlock =
  struct
    type de = Data | Empty
    type data = de ref
    let nul_block = ref Empty 
    and xor a b = if !b != Empty then if !a = Empty then a := !b else ()
  end;;
module NullDecoder = Incremental.Incremental_decoder(NullBlock);;
  
let graph = match !form with 
  MDS (logdata, irr, checks) -> 
    time "generate_MDS" (fun () -> Graphlib.generate_MDS logdata irr checks !impl)
| CID (data, checks) ->
    time "generate CID" (fun () -> Graphlib.generate_cid data checks !impl)
| REG (edges, data) -> 
    time "generate reg" (fun () -> Graphlib.generate_regular edges data !impl)
in
log#log 0 (sprintf "data = %d  checks = %d  trials = %d\n" graph#dcount graph#ccount !trials);
graph#print_checks 2;
let (description, action) = 
  match !act with
    DecodeProb -> 
      ("DecodeProb", fun () -> 
	for erase = 1 to graph#ccount + 1 do 
	  let realsuccess = (Graphlib.testreal graph !trials erase) and
	      ranksuccess = (Graphlib.testrank graph !trials erase) in
	  log#log 0 (sprintf "%d %d %d\n" erase realsuccess ranksuccess);
	done
      )
  | WorkNeeded -> 
      ("WorkNeeded", fun () -> 
	let sum = ref 0 in
	for t = 1 to !trials do
	  let decoder = new NullDecoder.graph_decoder graph
	  and blocks = Graphlib.randlist graph#all_blocks in
	  let rec decode_rec acc bl = 
	    if decoder#is_done then acc
	    else 
	      match bl with
	      [] -> assert false (* there is a problem if it can't decode given all blocks *)
	    | Graphlib.Check next :: rest -> 
		log#minor1 "Adding block C%d\n" next;
		decoder#add_check (next, ref NullBlock.Data);
		decode_rec (succ acc) rest
	    | Graphlib.Data next :: rest ->
		log#minor1 "Adding block D%d\n" next;
		decoder#add_data (next, ref NullBlock.Data);
		decode_rec (succ acc) rest
	  in
	  sum := !sum + decode_rec 0 blocks;
	  decoder#print_stats;
	  Gc.print_stat stdout;
	done;
	let avg = ((float_of_int !sum) /. (float_of_int !trials)) in
	let over = 100.0 *. (avg -. (float_of_int graph#dcount)) /. (float_of_int graph#dcount) in
	log#log 0 (sprintf "sum: %d trials: %d avg: %f %% over: %f\n" !sum !trials avg over)
      )
  | Freenet (_, simul) ->
      ("Freenet test", fun () -> 
	let sum = ref 0 in
	for t = 1 to !trials do
	  let decoder = new NullDecoder.want_decoder graph in
	  let queue = Queue.create() in
	  let queue_add = function None -> () | Some b -> Queue.add b queue in
	  for i = 1 to simul do queue_add decoder#wanted done;
	  (try 
	    while not decoder#is_done do
	      begin 
		decoder#add (Queue.take queue) (ref NullBlock.Data);
		incr sum; (* keep count *)
	      end;
	      queue_add decoder#wanted;  (* change to have prob != 1 *)
	    done;
	  with
	    Queue.Empty -> 
	      decoder#dump_status (); log#log 0 "Incomplete Decode; no more blocks requested\n"
	  );
	  decoder#print_stats;
	done;
	let avg = ((float_of_int !sum) /. (float_of_int !trials)) in
	let over = 100.0 *. (avg -. (float_of_int graph#dcount)) /. (float_of_int graph#dcount) in
	log#log 0 (sprintf "sum: %d trials: %d avg: %f %% over: %f\n" !sum !trials avg over)
      )
in
time description action;

