open Syntax
open EnvU
open EnvD
open Subst

(* most outer character is include upper function *)
(* envU = a:A, b:B 
   typing = {envU}, A 
   print typing = <typing> *)

let tyvar_to_latex tyvar = String.make 1 (Char.chr ((Char.code 'A')+ (tyvar mod 26)))

(* ty -> string *)
let rec type_to_latex ty = match ty with
  | TyUnit -> "\\mathrm{Unit}"
  | TyBool -> "\\mathrm{Bool}"
  | TyVar tyvar -> tyvar_to_latex tyvar
  | TyArr (ty1, ty2) -> "( " ^ (type_to_latex ty1) ^ " \\rightarrow " ^ (type_to_latex ty2) ^ " )" (* (A->B) *)

(* expvar -> ty -> string *)
let expvar_type_to_latex expvar ty = expvar ^ ": " ^ (type_to_latex ty) ^ ", "
(* task: delete last comma *)

(* envU -> string *)
(* without {} *)
let envU_to_latex envU = EnvU.fold (fun expvar ty string -> (expvar_type_to_latex expvar ty)^string) envU ""

(* expvar -> ty -> string *)
let tyvar_type_to_latex tyvar ty = (tyvar_to_latex tyvar) ^ " \\mapsto " ^ (type_to_latex ty) ^ ","
(* task: delete last comma *)

let subst_to_latex subst = Subst.fold (fun tyvar ty string -> (tyvar_type_to_latex tyvar ty)^string) subst ""

(* typing -> string *)
let typing_to_latex typing = match typing with 
  | (envU, ty) -> "\\{" ^ (envU_to_latex envU) ^ "\\}; " ^ (type_to_latex ty)  (* {x:u};u *)

let expcon_to_latex expcon = match expcon with
  | Unit -> "\\mathrm{unit}"
  | Bool -> "\\mathrm{bool}"
  | Arth -> " + "
  | Ifc -> "\\mathrm{Ifc}"

let rec exp_to_latex exp = match exp with
  | ExpCon c -> expcon_to_latex c
  | ExpVar x -> x
  | ExpAbs (x1, exp2) -> " (\\backslash " ^ x1 ^ "." ^ (exp_to_latex exp2) ^ ")"
  | ExpApp (exp1, exp2) -> "(" ^ (exp_to_latex exp1) ^ "\\," ^ (exp_to_latex exp2) ^ ")"
  | ExpRec (expvar1, exp2) -> " rec\\{ " ^ expvar1 ^ "=" ^ (exp_to_latex exp2) ^ " \\} "
  | ExpIf (exp1, exp2) -> "\\mathrm{if}(" ^ (exp_to_latex exp1) ^ "\\,|\\," ^ (exp_to_latex exp2) ^ ")"

let rec rules_to_latex rules = match rules with
  | [] -> ""
  | (ty1, ty2)::tl -> (type_to_latex ty1) ^ "=" ^ (type_to_latex ty2) ^ "," ^ (rules_to_latex tl)

(* new *)
(* expvar -> ty -> string *)
let expvar_typing_to_latex expvar typing = expvar ^ " :\\langle " ^ (typing_to_latex typing) ^ " \\rangle, "
(* task: delete last comma *)

let envD_to_latex envD = EnvD.fold (fun expvar typing string -> (expvar_typing_to_latex expvar typing)^string) envD ""

(* cond -> string *)
let condition_to_latex (envD, exp, typing) = 
  (envD_to_latex envD) ^ " \\vdash " ^ (exp_to_latex exp) ^ ": \\langle " ^ (typing_to_latex typing) ^ " \\rangle "

(* cond -> rules -> string *)
let condition_rules_to_latex cond rules = 
  (condition_to_latex cond) ^ " [" ^ (rules_to_latex rules) ^ "]"