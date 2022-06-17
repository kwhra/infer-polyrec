open Syntax
open EnvU
open EnvD
open Subst
open Variable
open Print
open Print_latex
open Basic_op
open Unify
open Inferlog
open Infertype
open Test

(* input envD and exp to test infering type *)
let testexp = exp1a
let testenvD = initD

(* input destination of log *)
let dstlog = 
	open_out "infer.log"

let () = 
(* show log *)
setinferlogopt (print_string("[print option] input 0-no log, 1-string, 2-latex: "); int_of_string(read_line()));
(* show rules in log *)
setruleopt (print_string("[log option] input 0-don't show rules, 1-show rules: "); int_of_string(read_line()));
(* you can change here the number k of rec *)
setreccount (print_string("[rec time] input rec time k: ");int_of_string (read_line ()));
(* you can infere the typing of the expression *)
(* "print (typing environment) (expression)" *)
print_string (condition_to_string (infertype testenvD testexp) ^ "\n");
output_string dstlog !inferlog
