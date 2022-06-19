open Syntax
open EnvU
open EnvD
open Subst

(* most outer character is include upper function *)
(* envU = a:A, b:B 
   typing = {envU}, A 
   print typing = <typing> *)

(* tyvar -> string *)
(* make length=1 string *)
(* 0->A, 1->B, ... *)
(* 26->A again. *)
let tyvar_to_string tyvar = String.make 1 (Char.chr ((Char.code 'A')+ (tyvar mod 26)))

(* ty -> string *)
let rec type_to_string ty = match ty with
  | TyCon tycon -> (match tycon with
                    | TyBool -> "Bool"
                    | TyUnit -> "Unit")
  | TyVar tyvar -> tyvar_to_string tyvar
  | TyArr (ty1, ty2) -> "(" ^ (type_to_string ty1) ^ " -> " ^ (type_to_string ty2) ^ ")" (* (A->B) *)

(* expvar -> ty -> string *)
let expvar_type_to_string expvar ty = expvar ^ ": " ^ (type_to_string ty) ^ ", "
(* task: delete last comma *)

(* envU -> string *)
(* without {} *)
let envU_to_string envU = EnvU.fold (fun expvar ty string -> (expvar_type_to_string expvar ty)^string) envU ""

(* expvar -> ty -> string *)
let tyvar_type_to_string tyvar ty = (tyvar_to_string tyvar) ^ " |-> " ^ (type_to_string ty) ^ ", "
(* task: delete last comma *)

let subst_to_string subst = Subst.fold (fun tyvar ty string -> (tyvar_type_to_string tyvar ty)^string) subst ""

(* typing -> string *)
let typing_to_string typing = match typing with 
  | (envU, ty) -> "{" ^ (envU_to_string envU) ^ "}; " ^ (type_to_string ty)  (* {x:u};u *)

let expcon_to_string expcon = match expcon with
  | Unit -> "unit"
  | Bool -> "bool"
  | Arth -> " + "
  | Ifc -> "Ifc"

let rec exp_to_string exp = match exp with
  | ExpCon c -> expcon_to_string c
  | ExpVar x -> x
  | ExpAbs (x1, exp2) -> "(\\" ^ x1 ^ ". " ^ (exp_to_string exp2) ^ ")"
  | ExpApp (exp1, exp2) -> "(" ^ (exp_to_string exp1) ^ " " ^ (exp_to_string exp2) ^ ")"
  | ExpRec (expvar1, exp2) -> " rec{" ^ expvar1 ^ " = " ^ (exp_to_string exp2) ^ "}"
  | _ -> "" (* todo *)

let rec rules_to_string rules = match rules with
  | [] -> ""
  | (ty1, ty2)::tl -> (type_to_string ty1) ^ " = " ^ (type_to_string ty2) ^ ", " ^ (rules_to_string tl)

(* expvar -> ty -> string *)
let expvar_typing_to_string expvar typing = expvar ^ ": <" ^ (typing_to_string typing) ^ ">, "
(* task: delete last comma *)

(* envD -> string *)
let envD_to_string envD = EnvD.fold (fun expvar typing string -> (expvar_typing_to_string expvar typing)^string) envD ""

(* cond -> string *)
let condition_to_string (envD, exp, typing) = 
  (envD_to_string envD) ^ " |- " ^ (exp_to_string exp) ^ ": <" ^ (typing_to_string typing) ^ ">"

(* cond -> rules -> string *)
let condition_rules_to_string cond rules = 
  (condition_to_string cond) ^ "\n where [" ^ (rules_to_string rules) ^ "]"