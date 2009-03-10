class logger pre glo = 
  object (self) 
    method log lev str =   
      if glo >= lev
      then begin prerr_string pre; prerr_string str; flush stderr; end
      else ()
    method log1 : int -> (int -> unit, out_channel, unit) format -> int -> unit = 
      fun lev form arg -> (* prints an stderr given a printf form with a single argument *)
	if glo >= lev 
	then begin prerr_string pre; Printf.eprintf form arg; end
	else ()
    method log2 : int -> (int -> int -> unit, out_channel, unit) format -> int -> int -> unit =
      fun lev form arg1 arg2 ->
	if glo >= lev
	then begin prerr_string pre; Printf.eprintf form arg1 arg2; end
	else ()
    method llog lev lazstr =  (* prints on stderr given a lazy string *)
      if glo >= lev
      then begin prerr_string pre; prerr_string (Lazy.force_val lazstr); flush stderr; end
      else ()
    method debug str = self#log 4 str
    method debug1 form arg = self#log1 4 form arg
    method ldebug lazstr = self#llog 4 lazstr
    method minor str = self#log 3 str
    method minor1 form arg = self#log1 3 form arg
    method lminor lazstr = self#llog 3 lazstr
    method normal str = self#log 2 str
    method normal1 form arg = self#log1 2 form arg
    method lnormal lazstr = self#llog 2 lazstr
    method info str = self#log 1 str
    method info1 form arg = self#log1 1 form arg
    method linfo lazstr = self#llog 1 lazstr
  end


let time pre doit str func = 
  if doit then 
    let start_time = Unix.gettimeofday () in
    let result = func () in
    Printf.eprintf "%s: %s took %f seconds\n" pre str ((Unix.gettimeofday ()) -. start_time);
    result
  else func ()
    
