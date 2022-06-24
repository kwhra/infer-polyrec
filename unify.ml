open Syntax
open EnvU
open EnvD
open Subst
open Variable
open Print
open Print_latex
open Baseop


exception CannotUnify
(* rules -> subst *)
let rec unify rules = match rules with
  | [] -> Subst.empty
  | (ty1, ty2)::tl -> 
    if ty1 = ty2
      then unify tl
    else if (is_tyvar ty1) && not (is_fv_in_ty (tyvar_of ty1) ty2)
      then (let subst = Subst.singleton (tyvar_of ty1) ty2 in
							merge_subst (unify (apply_subst_to_rules subst tl)) (subst))
    else if (is_tyvar ty2) && not (is_fv_in_ty (tyvar_of ty2) ty1)
      then (let subst = Subst.singleton (tyvar_of ty2) ty1 in
							merge_subst (unify (apply_subst_to_rules subst tl)) (subst))
    else if (is_tyarr ty1) && (is_tyarr ty2)
						then (match ty1 with TyArr (ty11, ty12) ->
                    (match ty2 with TyArr (ty21, ty22) ->
                      unify (tl @ [(ty11, ty21); (ty12, ty22)])
                      | _ -> raise CannotUnify) 
                    | _ -> raise CannotUnify )
    else raise CannotUnify

(* ty1 -> ty2 -> subst *)
let rec unify_singlerule ty1 ty2 = unify [(ty1, ty2)]
