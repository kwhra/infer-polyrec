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
let string_of_tyvar tyvar = String.make 1 (Char.chr ((Char.code 'A')+ (tyvar mod 26)))

(* ty -> string *)
let rec string_of_type ty = match ty with
  | TyCon tycon -> (match tycon with
                    | TyBool -> "Bool"
                    | TyUnit -> "Unit")
  | TyVar tyvar -> string_of_tyvar tyvar
  | TyArr (ty1, ty2) -> "(" ^ (string_of_type ty1) ^ " -> " ^ (string_of_type ty2) ^ ")" (* (A->B) *)

(* expvar -> ty -> string *)
let string_of_expvar_and_type expvar ty = expvar ^ ": " ^ (string_of_type ty) ^ ", "
(* task: delete last comma *)

(* envU -> string *)
(* without {} *)
let string_of_envU envU = EnvU.fold (fun expvar ty string -> (string_of_expvar_and_type expvar ty)^string) envU ""

(* expvar -> ty -> string *)
let string_of_tyvar_and_type tyvar ty = (string_of_tyvar tyvar) ^ " |-> " ^ (string_of_type ty) ^ ", "
(* task: delete last comma *)

let string_of_subst subst = Subst.fold (fun tyvar ty string -> (string_of_tyvar_and_type tyvar ty)^string) subst ""

(* typing -> string *)
let string_of_typing typing = match typing with 
  | (envU, ty) -> "{" ^ (string_of_envU envU) ^ "}; " ^ (string_of_type ty)  (* {x:u};u *)

let string_of_expcon expcon = match expcon with
  | Unit -> "unit"
  | Bool -> "bool"
  | Arth -> " + "
  | Ifc -> "Ifc"

let rec string_of_exp exp = match exp with
  | ExpCon c -> string_of_expcon c
  | ExpVar x -> x
  | ExpAbs (x1, exp2) -> "(\\" ^ x1 ^ ". " ^ (string_of_exp exp2) ^ ")"
  | ExpApp (exp1, exp2) -> "(" ^ (string_of_exp exp1) ^ " " ^ (string_of_exp exp2) ^ ")"
  | ExpRec (expvar1, exp2) -> " rec{" ^ expvar1 ^ " = " ^ (string_of_exp exp2) ^ "}"
  | _ -> "" (* todo *)

let rec string_of_rules rules = match rules with
  | [] -> ""
  | (ty1, ty2)::tl -> (string_of_type ty1) ^ " = " ^ (string_of_type ty2) ^ ", " ^ (string_of_rules tl)

(* expvar -> ty -> string *)
let string_of_expvar_and_typing expvar typing = expvar ^ ": <" ^ (string_of_typing typing) ^ ">, "
(* task: delete last comma *)

(* envD -> string *)
let string_of_envD envD = EnvD.fold (fun expvar typing string -> (string_of_expvar_and_typing expvar typing)^string) envD ""

(* cond -> string *)
let string_of_cond (envD, exp, typing) = 
  (string_of_envD envD) ^ " |- " ^ (string_of_exp exp) ^ ": <" ^ (string_of_typing typing) ^ ">"

(* cond -> rules -> string *)
let string_of_cond_and_rules cond rules = 
  (string_of_cond cond) ^ "\n where [" ^ (string_of_rules rules) ^ "]"

(* condtree -> string *)
let rec string_of_condtree condtree = 
  (* cond tree = cond * (cond tree) list *)
  (* print cond tree list first, then cond *)
  let Node (cond, childs) = condtree in
  let (envD, exp, typing) = cond in
  match childs with
  | [] -> 
    (match exp with
    | ExpVar _ -> "(Var)\n" 
    | ExpCon _ -> "(Con)\n" 
    | ExpAbs _ -> "(Abs)\n" 
    | ExpApp _ -> "(App)\n" 
    | ExpRec _ -> "(Rec)\n" 
    | ExpLet _ -> "(Let)\n" )
    ^ (string_of_cond cond) ^ "\n"
  (* hd:cond tree, tl:cond tree list *)
  | hd::tl -> (string_of_condtree hd) ^ (string_of_condtree (Node (cond, tl))) ^ "\n"
