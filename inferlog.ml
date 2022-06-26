open Syntax
open EnvU
open EnvD
open Subst
open Variable
open Print
open Print_latex
open Baseop

(* ref bool *)
(* false -> string, true -> latex *)
let printmode = ref false

(* unit -> unit *)
let set_latex() = printmode:=true
(* unit -> unit *)
let set_string() = printmode:=false

(* unit -> bool *)
let is_latex () = !printmode = false

(* envD -> exp -> typing -> rules -> unit *)
let log_of_condtree condtree = 
	if is_latex() then string_of_condtree condtree
								else latex_of_condtree condtree
