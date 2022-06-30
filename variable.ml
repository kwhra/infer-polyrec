(* ty variable and exp variable  *)

open Syntax
open EnvU
open EnvD
open Subst

(* reference: https://www.fos.kuis.kyoto-u.ac.jp/~igarashi/class/isle4-06w/text/miniml011.html *)
let tyvar_counter = ref 0

(* unit -> tyvar *)
(* get current tyvar, then increment *)
let get_fleshtyvar () = 
  let value = !tyvar_counter in
    (tyvar_counter := value+1;value)

(* unit -> tyvar *)
(* just get current tyvar *)
let get_current_tyvar () = !tyvar_counter

(* tyvar -> unit *)
let set_current_tyvar tyvar = tyvar_counter := tyvar

(* unit -> unit *)
let reset_counter () = tyvar_counter := 0


(* 'a -> 'a list -> 'a list *)
let rec delete elm ls = match ls with
  | [] -> ls
  | hd::tl -> if hd = elm then delete elm tl
                          else hd::(delete elm tl)

(* expression -> expvar list *)
let rec fvs_in_exp exp = match exp with
  | ExpVar x -> [x]
  | ExpCon _ -> []
  | ExpAbs (x, exp1) -> delete x (fvs_in_exp exp1)
  | ExpApp (exp1, exp2) -> (fvs_in_exp exp1) @ (fvs_in_exp exp2) 
  | ExpRec (expvar, exp2) -> delete expvar (fvs_in_exp exp2)
  | ExpLet (expvar, exp2, exp3) -> (fvs_in_exp exp2)@(delete expvar (fvs_in_exp exp3))

(* expvar -> expression -> bool *)
let is_fv_in_exp x exp = List.mem x (fvs_in_exp exp)

(* ty -> tyvar list *)
let rec fvs_in_ty ty = match ty with
  | TyVar tyvar -> [tyvar]
  | TyArr (ty1, ty2) -> (fvs_in_ty ty1) @ (fvs_in_ty ty2)
  | _ -> []

(* tyvar -> ty -> bool *)
let is_fv_in_ty tyvar ty = List.mem tyvar (fvs_in_ty ty)

(* new: *)
(* envU -> expvar list *)
let domU envU = List.map (fun (key, _) -> key) (EnvU.bindings envU)
let domD envD = List.map (fun (key, _) -> key) (EnvD.bindings envD)

let is_in_domD x envD = EnvD.mem x envD

(* envD -> expvar list *)
let vr_in_D envD = List.fold_right (fun envU list -> (domU envU)@list) (List.map (fun (_, (envU, _)) -> envU) (EnvD.bindings envD)) []

(* ls -> ls > ls *)
let diff ls1 ls2 = List.fold_right delete ls2 ls1

(* exp -> envD -> expvar list *)
let fvd_in_exp exp envD = (diff (fvs_in_exp exp) (domD envD))
                            @(vr_in_D (EnvD.filter (fun key _ -> is_fv_in_exp key exp) envD))

let is_in_fvd_in_exp expvar exp envD = List.mem expvar (fvd_in_exp exp envD)

let is_in_D expvar envD = List.mem expvar ((domD envD)@(vr_in_D envD))

(* ty -> bool *)
let is_tyarr ty = match ty with
  | TyArr (_, _) -> true
  | _ -> false

(* ty -> bool *)
let is_tyvar ty = match ty with
  | TyVar _ -> true
  | _ -> false

exception NotTyvar
let tyvar_of ty = 
  if is_tyvar ty
    then (match ty with TyVar tyvar -> tyvar | _ -> raise NotTyvar)
    else raise NotTyvar
