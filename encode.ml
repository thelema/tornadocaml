open Printf
open Arg2
open Graphlib

exception Unknown_Type
exception Unknown_Form

type graph_type = 
    MDS of int * int * int
  | CID of int * int
  | REG of int * int
  | TOR of int * int * int
type mode = 
    Encode
  | Decode
  | Crush1 (* removes random data blocks from a TarFile *)
  | Crush2 (* removes random data and check blocks from a TarFile *)
type data_location_type = 
    TarFile of string
  | File    of string
  | FileSeq of string
  | StdIO

module WeakFile = 
  struct 
    type t = int * Pervasives.in_channel * string Weak.t
(*    let header_len = ref *)
    let get (bs, ic, wa) i = 
      match Weak.get wa i with 
	Some data -> data
      | None -> 
	  seek_in ic (bs * i); 
	  let str = String.make bs '\000' in 
	  let _ = input ic str 0 bs in 
	  Weak.set wa i (Some str);
	  str
(*    let get_pos ((bs, ic, wa) as item) pos len = 
      if len > bs then raise (Invalid_argument "WeakFile.get_pos");
      let i = pos / bs in
      get item i *) (* finish me? *)
    let of_name fname bs = 
      let fh = open_in_bin fname in 
      let size = in_channel_length fh in
      (bs, fh, Weak.create (1+(size / bs)))
    let block_size (bs, _, _) = bs
    let length (_, _, wa) = Weak.length wa
    let size (_, ic, _) = in_channel_length ic
  end

module FileBlock =
  struct
    type t = SA of string | IA of Int32.t array
    let xor_sa a b = 
      let la = String.length a in
      assert (la = String.length b);
	let out = String.create la in
	for i = 0 to la-1 do
	  out.[i] <- Char.chr ((Char.code a.[i]) lxor (Char.code b.[i]));
	done;
	out
    let xor_ia a b = 
      let la = Array.length a in
      assert (la = Array.length b);
      Array.init la (fun i -> Int32.logxor a.(i) b.(i))
	
    let to_int32arr a =
      let al = String.length a in
      let to_int32 s = (* turns a string of length 4 into an Int32 *)
	Int32.logor (Int32.of_int (Char.code s.[4])) 
	  (Int32.logor (Int32.shift_left (Int32.of_int (Char.code s.[3])) 8)
	     (Int32.logor (Int32.shift_left (Int32.of_int (Char.code s.[2])) 16)
		(Int32.shift_left (Int32.of_int (Char.code s.[1])) 24)))
	  (* Phew!  That's a mouthful to turn a 4 bytes into an identical 32 bit value *)
      in 
      let int32_of_pos p = 
	let p4 = p * 4 in
	let sub = 
	  if p4 + 4 > al then String.sub a p4 (al-p4)
	  else String.sub a p4 4 in
	to_int32 sub in
      Array.init ((al+3)/4) int32_of_pos 
    let to_string a =
      let al = Array.length a in
      let to_char int pos =
	(Int32.to_int (Int32.logand 255l (Int32.shift_right int (pos * 8))))
      in
      Array.init (al * 4) (fun i -> to_char a.(i/4) (i mod 4))

    let xor a b = match (a, b) with
      SA a, SA b -> SA (xor_sa a b)
    | SA a, IA b -> IA (xor_ia (to_int32arr a) b)
    | IA a, SA b -> IA (xor_ia a (to_int32arr b))
    | IA a, IA b -> IA (xor_ia a b)

  end

type block_metadata = Data of int | Check of int list
type block = block_metadata * Fileblock.t

(*module WeakBlockfile =
struct
type t = *)

module FileID =
  struct 
    type t = string
    let to_string x = x
    let compare a b = Pervasives.compare a b
    let zero = "0"
    let next i = string_of_int (1+(int_of_string i))
  end

module FileDecoder = Incremental.Incremental_decoder(FileBlock)(FileID)(FileID)

let main () = 
  Random.self_init();
  let log = new Log.logger "TestGraph: " 1
  and time str func = Log.time "TestGraph" true str func in
  
(*  let read_file name = 
    let fh = open_out_bin name in
    *)
    
  let act = ref Encode
  and form = ref (MDS (8,29,120))
  and trials = ref 20
  and size = ref 2048
  and source = ref (File "testin")
  and destination = ref (TarFile "testout")
  and int1 = ref 0  (* for holding values of arguments to -m and -c *)
  and int2 = ref 0
  and int3 = ref 0
  and flt1 = ref 0.0 in

  let args =
    [ (Both ('t', "trials"), [Int_var trials], [], 
       "Set number of trials to do");
      (Both ('d', "decode"), [Unit (fun () -> act := Decode)], 
       [String (fun s -> source := if s = "-" then StdIO else TarFile s), 
	(fun () -> source := StdIO) ],
       "Decode input data");
      (Both ('e', "encode"), [Unit (fun () -> act := Encode)], 
       [String (fun s -> source := if s = "-" then StdIO else File s), 
	(fun () -> source := StdIO) ],
       "Encode input data");
      (Both ('c', "crush1"), [Unit (fun () -> act := Crush1)], 
       [String (fun s -> source := if s = "-" then StdIO else TarFile s), 
	(fun () -> source := StdIO) ],
       "Crush a TarFile (removing random data blocks)");
      (Both ('C', "crush2"), [Unit (fun () -> act := Crush2)], 
       [String (fun s -> source := if s = "-" then StdIO else TarFile s), 
	(fun () -> source := StdIO) ],
       "Crush a TarFile (removing random check and data blocks)");

      (Both_extra ('s', "size", "size"), [Int_var size], [], 
       "Set the block size (in bytes)");
      
      (Both_extra ('m', "mds", "[logdata] [irr] [checks]"), [],
       [
	(Int_var int1, (fun () -> int1 := 8));
	(Int_var int2, (fun () -> int2 := 29));
	(Int_var int3, (fun () -> int3 := 120));
	(String (fun _ -> form := MDS (!int1, !int2, !int3)),
	 (fun () -> form := MDS (!int1, !int2, !int3)))],
       "Set graph type to mds");
      (Both_extra ('T', "tor", "[d] [data] [checks]"), [],
       [
	(Int_var int1, (fun () -> int1 := 10));
	(Int_var int2, (fun () -> int2 := 1000));
	(Int_var int3, (fun () -> int3 := 500));
	(String (fun _ -> form := TOR (!int1, !int2, !int3)),
	 (fun () -> form := TOR (!int1, !int2, !int3)))],
       "Set graph type to tornado");
      (Both_extra ('r', "reg", "[edges] [data]"), [],
       [
	(Int_var int1, (fun () -> int1 := 50));
	(Int_var int2, (fun () -> int2 := 25));
	(String (fun _ -> form := CID (!int1, !int2)),
	 (fun () -> form := CID (!int1, !int2)))],
       "Set graph type to regular");
      (Both_extra ('c', "cid", "[data] [checks]"), [],
       [
	(Int_var int1, (fun () -> int1 := 31));
	(Int_var int2, (fun () -> int2 := 7));
	(String (fun _ -> form := CID (!int1, !int2)),
	 (fun () -> form := CID (!int1, !int2)))],
       "Set graph type to cid")]
  and usage_info = "testgraph [parameters]"
  and descr = "Encode/decode FEC"
  and notes = "by Eric Norige" in
  Arg2.parse args (fun _ -> ()) usage_info descr notes;
  
  let datas = 5 in
  match !act with
    Encode -> 
      let graph = match !form with 
	MDS (logdata, irr, checks) -> 
	  time "generate_MDS" (fun () -> generate_MDS logdata irr checks !impl)
      | CID (data, checks) ->
	  time "generate CID" (fun () -> generate_cid data checks !impl)
      | REG (data, checks) -> 
	  time "generate Regular" (fun () -> generate_regular data checks !impl)
      | TOR (d, data, checks) ->
	  time "generate Tornado" (fun () -> generate_tornado d data checks !impl)
      in
      log#log 0 (sprintf "data = %d  checks = %d edges = %d trials = %d\n" graph#dcount graph#ccount graph#edges !trials)
	
	
let _ = main ()
