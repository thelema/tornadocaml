let log = new Log.logger "BFField: " 1

exception Not_irreducible;;  
          (* raised if the (n,irr) parameters don't form 
	     an irreducible field *)
type elem = Pow of int | Sum of int;;

class bffield n irr =
  let q = 1 lsl n in
  let r = q + irr in
  let p2s = let out = Array.create q 0 in
  out.(0) <- 1;
  out in
  let s2p = 
    let out = Array.create q 0 in
    out.(0) <- -1; out.(1) <- 0;
    for i=1 to q-2 do
      let num = p2s.(i-1) lsl 1 in
      let entry = if num < q then num else num lxor r in
      if entry = 1 then raise Not_irreducible;
      p2s.(i) <- entry;
      out.(entry) <- i;
    done;
    out in
object (f)
  method sum = function
    Sum x -> x 
  | Pow (-1) -> 0 
  | Pow x -> p2s.(x)
  method pow = function
    Pow x -> x 
  | Sum 0 -> -1 
  | Sum x -> s2p.(x)
  method out = function 
    Sum x -> x 
  | Pow (-1) -> 0 
  | Pow x -> p2s.(x)
  method add x y = f#addsum (f#sum x) (f#sum y)
  method mult x y = f#multpow (f#pow x) (f#pow y)
  method private multpow (x: int) (y: int) = (* both x and y are in pow form *)
      if x = -1 or y = -1 then (Pow (-1)) (* 0 * x = 0 *)
      else (Pow ((x+y) mod q))
  method private addsum (x: int) (y: int) = (* both x and y are in sum form *)
    Sum (x lxor y)
  method mds_poly k = let poly = Array.create (k+1) (Sum 0) in
  poly.(0) <- (Sum 1);
  for i = 0 to k-1 do
    for j = i+1 downto 1 do
      poly.(j) <- f#add poly.(j) (f#mult (Pow i) poly.(j-1))
    done
  done;
  poly
  method mds_array rows =
    let k = q - rows - 1 in
    let poly = f#mds_poly k in
    let result = Array.create_matrix rows (q-1) (Sum 0) in
    log#info (Printf.sprintf "rows: %d k: %d\n" rows k);
    for i = 0 to rows-1 do
      Array.blit poly 0 result.(i) i (k+1)
    done;
    result
end;;
