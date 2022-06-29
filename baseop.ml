open Syntax
open EnvU
open EnvD
open Subst

(* substitution, append, add, ... on Env, Sub, and rules *)

(* envU -> envU -> envU *)
(* x:t1 in U1, x:t2 in U2 =>  x:t1 in U1U2*)
let merge_envU envU1 envU2 = 
  EnvU.merge 
    (fun _ ty1 ty2 ->
      match ty1, ty2 with
      | Some x, Some y -> Some x 
      | Some x, None -> Some x
      | None, Some y -> Some y
      | None, None -> None)
    envU1
    envU2

(* envD -> envD -> envD *)
let merge_envD envD1 envD2 = 
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

(* tyvar -> ty -> type -> type *)
(* look type struct, then substitute *)
let rec apply_singlesubst_to_type tyvar11 ty12 ty2 = match ty2 with
  | TyVar tyvar21 -> if tyvar11 = tyvar21 then ty12 else TyVar tyvar21
  | TyArr (ty21, ty22) -> TyArr ((apply_singlesubst_to_type tyvar11 ty12 ty21), (apply_singlesubst_to_type tyvar11 ty12 ty22))
  | _ -> ty2

(* subst -> type -> type *)
let rec apply_subst_to_type subst ty =
  Subst.fold
  (apply_singlesubst_to_type)
  subst
  ty

(* subst -> envU -> envU *)
let apply_subst_to_envU subst envU = 
  EnvU.map
  (apply_subst_to_type subst) (* :ty -> ty *)
  envU

(* tyvar -> ty -> envU ->envU *)
let apply_singlesubst_to_envU  tyvar ty envU = 
  apply_subst_to_envU 
  (Subst.add tyvar ty Subst.empty)
  envU

(* sub -> sub -> sub *)
let apply_subst_to_subst subst sub = 
  Subst.map
  (apply_subst_to_type subst) (* :ty -> ty *)
  sub

(* sub -> sub -> sub *)
let merge_subst sub1 sub2 =
  Subst.merge
    (fun _ ty1 ty2 -> 
      match ty1, ty2 with
      | Some x, Some y -> Some x
      | Some x, None -> Some x
      | None, Some y -> Some y
      | None, None -> None)
    (* substs are merged unintentionally order *)
    (apply_subst_to_subst sub2 sub1)
    (apply_subst_to_subst sub1 sub2)

(* subst -> typing -> typing *)
let apply_subst_to_typing subst typing = match typing with (envU, ty) -> 
                                ((apply_subst_to_envU subst envU), (apply_subst_to_type subst ty))

(* subst -> rules -> rules *)
let rec apply_subst_to_rules subst rules = 
  match rules with
  | [] -> []
  (* rules = (ty * ty) list *)
  | (ty1, ty2)::tl -> (apply_subst_to_type subst ty1, apply_subst_to_type subst ty2)::(apply_subst_to_rules subst tl)

(* subst -> envD -> envD *)
let apply_subst_to_envD subst envD = 
  EnvD.map
  (apply_subst_to_typing subst) (* :typing -> typing *)
  envD

(* subst -> cond -> cond *)
let apply_subst_to_cond subst cond = 
  let (envD, exp, typing) = cond in
  (apply_subst_to_envD subst envD, exp, apply_subst_to_typing subst typing)

(* ('a -> 'b) -> 'a tree = 'b tree *)
let rec map_to_tree func sometree = 
  let Node (some, childs) = sometree in
  match childs with
  | [] -> Node (func some, [])
  (* hd: 'a tree, tl: 'a tree list *)
  | hd::tl ->
    let newhead = map_to_tree func hd in
    let Node (newsome, newtl) = map_to_tree func (Node(some, tl)) in
    Node (newsome, newhead::newtl)

(* subst -> cond tree -> cond tree *)
let apply_subst_to_condtree subst condtree = map_to_tree (apply_subst_to_cond subst) condtree

(* (cond, rules) tree -> cond tree *)
let removerules condrulestree = map_to_tree (fun (cond, _ )-> cond) condrulestree

(* subst -> condrulestree -> condrulestree *)
let apply_subst_to_condrulestree subst condrulestree = map_to_tree (fun(cond, rules) -> (apply_subst_to_cond subst cond, [])) condrulestree

(* ('a -> 'b -> 'b) -> 'a tree -> 'b -> 'b *)
(* func:'a -> 'b -> 'b, tree:'a tree, init:'b *)
let rec fold_tree func tree init = 
  (* some:'a, childs:'a tree list *)
  let Node (some, childs) = tree in
  match childs with 
  | [] -> func some init
  | hd::tl ->
    let newhead = fold_tree func hd init in
    let Node (newsome, newtl) = fold_tree func (Node (some, tl)) init in
     Node (newsome, newhead::newtl)