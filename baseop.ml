open Syntax
open EnvU
open EnvD
open Subst

(* substitution, append, add, ... on ENv, Sub, and rules *)

(* expvar -> type -> envU -> envU *)
let envUappend expvar ty envU = EnvU.add expvar ty envU 

(* expvar -> type -> envU *)
let singleenvU expvar ty = EnvU.singleton expvar ty

(* envU -> envU -> envU *)
(* x:t1 in U1, x:t2 in U2 =>  x:t1 in U1U2*)
let envUmerge envU1 envU2 = 
  EnvU.merge 
    (fun _ ty1 ty2 ->
      match ty1, ty2 with
      | Some x, Some y -> Some x 
      | Some x, None -> Some x
      | None, Some y -> Some y
      | None, None -> None)
    envU1
    envU2

exception CannotMergeEnvD
(* envD -> envD -> envD *)
let envDmerge envD1 envD2 = 
  EnvD.merge 
    (fun _ typing1 typing2 -> 
      match typing1, typing2 with
      | Some x, Some y -> Some x (* todo *)
      | Some x, None -> Some x
      | None, Some y -> Some y
      | None, None -> None) 
    envD1
    envD2

(* envU -> envU -> envU *)
(* if x:A and x:B->C, the list is [(A, B->C)] *)
let samekey_in envU1 envU2 = 
  EnvU.merge
    (fun _ ty1 ty2 -> 
      match ty1, ty2 with
      | Some x, Some y -> Some(x, y) 
      | Some x, None -> None
      | None, Some y -> None
      | None, None -> None)
    envU1
    envU2

(* envU -> envU -> (key * (ty*ty)) list  *)
let samekeylist_in envU1 envU2 = EnvU.bindings (samekey_in envU1 envU2)

(* (key * (ty*ty)) list -> (ty*ty) list *)
let rec deletekey ls = match ls with 
  | [] -> []
  | (_, (ty1, ty2))::tl -> (ty1, ty2)::(deletekey tl)

(* envU -> envU -> (ty*ty) list *)
let rules_of_samekey_in envU1 envU2 = deletekey (samekeylist_in envU1 envU2)

(* tyvar -> ty -> subst -> subst *)
let substappend tyvar ty subst = Subst.add tyvar ty subst

(* tyvar -> ty -> sub *)
let singlesubst tyvar ty = Subst.singleton tyvar ty

(* tyvar -> ty -> type -> type *)
(* look type struct, then substitute *)
let rec typesinglesubst tyvar11 ty12 ty2 = match ty2 with
  | TyVar tyvar21 -> if tyvar11 = tyvar21 then ty12 else TyVar tyvar21
  | TyArr (ty21, ty22) -> TyArr ((typesinglesubst tyvar11 ty12 ty21), (typesinglesubst tyvar11 ty12 ty22))
  | _ -> ty2

(* subst -> type -> type *)
let rec typesubst subst ty =
  Subst.fold
  (typesinglesubst)
  subst
  ty

(* subst -> envU -> envU *)
let envUsubst subst envU = 
  EnvU.map
  (typesubst subst) (* :ty -> ty *)
  envU

(* tyvar -> ty -> envU ->envU *)
let envUsinglesubst  tyvar ty envU = 
  envUsubst 
  (Subst.add tyvar ty Subst.empty)
  envU

(* sub -> sub -> sub *)
let substsubst subst sub = 
  Subst.map
  (typesubst subst) (* :ty -> ty *)
  sub

let typingsubst subst typing = match typing with (envU, ty) -> 
                                ((envUsubst subst envU), (typesubst subst ty))

(* sub -> sub -> sub *)
let substmerge sub1 sub2 =
  Subst.merge
    (fun _ ty1 ty2 -> 
      match ty1, ty2 with
      | Some x, Some y -> Some x
      | Some x, None -> Some x
      | None, Some y -> Some y
      | None, None -> None)
    (* substs are merged unintentionally order *)
    (substsubst sub2 sub1)
    (substsubst sub1 sub2)

(* subst -> rules -> rules *)
let rec rulessubst subst c = match c with
  | [] -> []
  | (ty1, ty2)::tl -> ((typesubst subst ty1), (typesubst subst ty2))::(rulessubst subst tl)
