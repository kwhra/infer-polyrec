open Syntax
open EnvU
open EnvD
open Subst
open Variable
open Print
open Print_latex
open Basic_op

(* ref int *)
(* 0 -> false, 1 -> string, 2 -> latex *)
let inferlogopt = ref 0

(* int -> unit *)
let setinferlogopt int = inferlogopt:=int

(* unit -> bool *)
let nologmode () =
	if (!inferlogopt = 0) then true
												else false
(* unit -> bool *)
let stringmode () =
	if (!inferlogopt = 1) then true
												else false
(* unit -> bool *)
let latexmode () =
	if (!inferlogopt = 2) then true
												else false

let rulelogopt = ref 0
let setruleopt int = rulelogopt:=int
let visiblerules () = 
	if !rulelogopt = 1 then true
	else false

(* ref string *)
let inferlog = ref ""

(* string -> unit *)
let writeinferlog log = 
  let templog = !inferlog in
    (inferlog := templog ^ log)

(* unit -> string *)
let getlog () = !inferlog

(* unit -> unit *)
let resetlog() = (inferlog := "")

(* envd -> exp -> typing -> rules -> string *)
(* ex: *)
(* (var) *)
(* D|-e:<;u> *)
let gen_string envD exp typing rules = 
	let label = match exp with
			| ExpVar _ -> "(Var)"
			| ExpCon _ -> "(Con)"
			| ExpAbs _ -> "(Abs)"
			| ExpApp _ -> "(App)"
			| ExpRec _ -> "(Rec)"
			| ExpIf _ -> "(If)"
	in if visiblerules()
		then label ^ "\n" ^ (condition_rules_to_string (envD, exp, typing) rules) ^ "\n"
		else label ^ "\n" ^ (condition_to_string (envD, exp, typing)) ^ "\n"

(* encd -> exp -> typing -> rules -> string *)
(* ex: *)
(*  *)
(* ------------(var) *)
(* D|-e:<x:u; u> *)
let gen_latex envD exp typing rules = 
	let ruleslog = 
		(if visiblerules() then ", where [" ^ (rules_to_latex rules) ^ "]"
		else "") in
	let cond = (envD, exp, typing) in 
		match exp with
		| ExpVar _ -> 
			"\\AxiomC{}\n" ^ "\\RightLabel{(Var)}\n" ^ "\\UnaryInfC{$" ^ (condition_to_latex cond) ^ ruleslog ^ "$}\n"
		| ExpCon _ -> 
			"\\AxiomC{}\n" ^ "\\RightLabel{(Con)}\n" ^ "\\UnaryInfC{$" ^ (condition_to_latex cond) ^ ruleslog ^ "$}\n"
		| ExpAbs _ -> 
										 	 "\\RightLabel{(Abs)}\n" ^ "\\UnaryInfC{$" ^ (condition_to_latex cond) ^ ruleslog ^ "$}\n"
		| ExpApp _ -> 
										   "\\RightLabel{(App)}\n" ^ "\\BinaryInfC{$" ^ (condition_to_latex cond) ^ ruleslog ^ "$}\n"
		| ExpRec _ ->
			"\\AxiomC{}\n" ^ "\\RightLabel{(Rec)}\n" ^ "\\UnaryInfC{$" ^ (condition_to_latex cond) ^ ruleslog ^ "$}\n"
		| ExpIf _ ->
										   "\\RightLabel{(If)}\n" ^ "\\BinaryInfC{$" ^ (condition_to_latex cond) ^ ruleslog ^ "$}\n"

(* envD -> exp -> typing -> rules -> unit *)
let geninferlog envD exp typing rules = 
	if nologmode() then ()
	else if stringmode() then writeinferlog (gen_string envD exp typing rules)
	else writeinferlog (gen_latex envD exp typing rules)

let genunifylog rules subst  = 
	if nologmode() then ()
	else if stringmode() then writeinferlog ( "where " ^ (rules_to_string rules) ^ "\nunify\n" ^ (subst_to_string subst) ^ "\n" )
	else writeinferlog ( "\nwhere $" ^ (rules_to_latex rules) ^ "$\n\nunify\n\n$" ^ (subst_to_latex subst) ^ "$\n\n" )