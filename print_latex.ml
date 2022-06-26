open Syntax
open EnvU
open EnvD
open Subst

(* most outer character is include upper function *)
(* envU = a:A, b:B 
   typing = {envU}, A 
   print typing = <typing> *)

let latex_of_tyvar tyvar = String.make 1 (Char.chr ((Char.code 'A')+ (tyvar mod 26)))

(* ty -> string *)
let rec latex_of_type ty = match ty with
  | TyCon tycon -> (match tycon with
                    | TyBool -> "\\mathrm{Bool}"
                    | TyUnit -> "\\mathrm{Unit}")
  | TyVar tyvar -> latex_of_tyvar tyvar
  | TyArr (ty1, ty2) -> "( " ^ (latex_of_type ty1) ^ " \\rightarrow " ^ (latex_of_type ty2) ^ " )" (* (A->B) *)

(* expvar -> ty -> string *)
let latex_of_expvar_and_type expvar ty = expvar ^ ": " ^ (latex_of_type ty) ^ ", "
(* task: delete last comma *)

(* envU -> string *)
(* without {} *)
let latex_of_envU envU = EnvU.fold (fun expvar ty string -> (latex_of_expvar_and_type expvar ty)^string) envU ""

(* expvar -> ty -> string *)
let latex_of_tyvar_and_type tyvar ty = (latex_of_tyvar tyvar) ^ " \\mapsto " ^ (latex_of_type ty) ^ ","
(* task: delete last comma *)

let latex_of_subst subst = Subst.fold (fun tyvar ty string -> (latex_of_tyvar_and_type tyvar ty)^string) subst ""

(* typing -> string *)
let latex_of_typing typing = match typing with 
  | (envU, ty) -> "\\{" ^ (latex_of_envU envU) ^ "\\}; " ^ (latex_of_type ty)  (* {x:u};u *)

let latex_of_expcon expcon = match expcon with
  | Unit -> "\\mathrm{unit}"
  | Bool -> "\\mathrm{bool}"
  | Arth -> " + "
  | Ifc -> "\\mathrm{Ifc}"

let rec latex_of_exp exp = match exp with
  | ExpCon c -> latex_of_expcon c
  | ExpVar x -> x
  | ExpAbs (x1, exp2) -> " (\\backslash " ^ x1 ^ "." ^ (latex_of_exp exp2) ^ ")"
  | ExpApp (exp1, exp2) -> "(" ^ (latex_of_exp exp1) ^ "\\," ^ (latex_of_exp exp2) ^ ")"
  | ExpRec (expvar1, exp2) -> " rec\\{ " ^ expvar1 ^ "=" ^ (latex_of_exp exp2) ^ " \\} "
  | _ -> ""(* todo: let *)

let rec latex_of_rules rules = match rules with
  | [] -> ""
  | (ty1, ty2)::tl -> (latex_of_type ty1) ^ "=" ^ (latex_of_type ty2) ^ "," ^ (latex_of_rules tl)

(* new *)
(* expvar -> ty -> string *)
let latex_of_expvar_typing expvar typing = expvar ^ " :\\langle " ^ (latex_of_typing typing) ^ " \\rangle, "
(* todo: delete last comma *)

let latex_of_envD envD = EnvD.fold (fun expvar typing string -> (latex_of_expvar_typing expvar typing)^string) envD ""

(* cond -> string *)
let latex_of_cond (envD, exp, typing) = 
  (latex_of_envD envD) ^ " \\vdash " ^ (latex_of_exp exp) ^ ": \\langle " ^ (latex_of_typing typing) ^ " \\rangle "

(* cond -> rules -> string *)
let condition_rules_to_latex cond rules = 
  (latex_of_cond cond) ^ " [" ^ (latex_of_rules rules) ^ "]"
