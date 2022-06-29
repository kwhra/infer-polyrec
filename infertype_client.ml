open Syntax
open EnvU
open EnvD
open Subst
open Variable
open Print
open Print_latex
open Baseop
open Unify
open Inferlog
open Rename
open Infertype
open Test

(* input envD and exp to test infering type *)
let testexp = exp0
let testenvD = initD

(* input destination of log *)
let dstlog = 
	open_out "infer.log"

let () = 
(* show log *)
print_string ("[print option] input 1-string, 2-latex: "); 
	let input =  int_of_string(read_line()) in
 	if input = 1 then set_string() else set_latex();
(* you can change here the number k of rec *)
print_string ("[rec time] input rec time k: ");
	setreccount (int_of_string (read_line ()));
(* you can infere the typing of the expression *)
(* "print (typing environment) (expression)" *)
let tree = infertype testenvD testexp in
let Node(cond, _) = tree in
print_string ((string_of_cond cond) ^ "\n");
output_string dstlog (log_of_condtree tree)
