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
let testexp = ex3
let testenvD = initD

(* input destination of log *)
let dst_log = 
	open_out "infer.log"
let dst_log_unified = 
	open_out "infer_unified.log"

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
let (tree, unifiedtree) = infertype testenvD testexp in
let Node(cond, _) = unifiedtree in
print_string ((string_of_cond cond) ^ "\n");
output_string dst_log (log_of_condtree tree);
output_string dst_log_unified (log_of_condtree unifiedtree)