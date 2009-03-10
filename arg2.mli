(* O'Caml enhanced command line argument handling module            *)
(* by Travis Bemann and Eric Norige                                 *)
(*                                                                  *)
(* This program is free software; you can redistribute it and/or    *)
(* modify it under the terms of the GNU Lesser General Public       *)
(* License as published by the Free Software Foundation; either     *)
(* version 2 of the License, or (at your option) any later version. *)
(*                                                                  *)
(* This program is distributed in the hope that it will be useful,  *)
(* but WITHOUT ANY WARRANTY; without even the implied warranty of   *)
(* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the    *)
(* GNU Lesser General Public License for more details.              *)

type spec =
    Unit of (unit -> unit)     (* Call the function with unit argument *)
  | Set of bool ref            (* Set the reference to true *)
  | Clear of bool ref          (* Set the reference to false *)
  | String of (string -> unit) (* Call the function with a string argument *)
  | Int of (int -> unit)       (* Call the function with an int argument *)
  | Float of (float -> unit)   (* Call the function with a float argument *)
  | String_var of string ref   (* Set the reference to the string argument *)
  | Int_var of int ref         (* Set the reference to the int argument *)
  | Float_var of float ref     (* Set the reference to the float argument *)
  | Rest of (string -> unit);; (* Stop interpreting keywords and call the
                                  function with each remaining argument *)

type switch =
    Char of char               (* Character switch *)
  | Char_arg of char * string  (* Character switch with argument name *)
  | Char_extra of char * string
      (* Character switch with extra non-simple argument information,
       * usually final argument list information *)
  | Name of string             (* Name switch *)
  | Name_arg of string * string
      (* Name switch with argument name *)
  | Name_extra of string * string
      (* Name switch with extra non-simple argument information,
       * usually final argument list information *)
  | Both of char * string      (* Both character and name switch *)
  | Both_arg of char * string * string
      (* Both character and name switch with argument name *)
  | Both_extra of char * string * string
      (* Both character and name switch with extra non-simple argument
       * information, usually final argument list information *)

exception Bad of string        (* Raise this to signal an error
			          message during/in response to
			          argument parsing; this will cause
			          Parse_error to be raised in turn *)
exception Parse_halt           (* Catch this to signal that argument
			          parsing has failed OR otherwise
			          halted; program should exit
			          immediately *)

val parse : 
    keywords:(switch * spec list * (spec * (unit -> unit)) list *string) list ->
      others:(string -> unit) -> 
	usage:string -> 
	  descr:string ->
            notes:string -> 
	      unit

val usage : 
    keywords:(switch * spec list * (spec * (unit -> unit)) list *string) list ->
      usage:string -> 
	descr:string -> 
	  notes:string -> 
	    unit
