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

exception NotDefined

(* type(c) : expcon -> ty *)
let consttype_of c = match c with
  | Unit -> TyCon TyUnit
  | Bool -> TyCon TyBool
  | Ifc -> let u = TyVar (getfleshtyvar()) in
              TyArr(TyCon TyBool, TyArr(u, TyArr(u, u)))
  | Arth -> let u = TyVar (getfleshtyvar()) in
              TyArr(u, TyArr(u, u))

(* expvar list -> envU *)
let envU_of_Bx ls = 
  List.fold_right 
    (fun expvar envU -> EnvU.add expvar (TyVar (getfleshtyvar())) envU)
    ls
    EnvU.empty

(* ty -> ty *)
let rec rename ty = 
  let renamecounter = ref 0 in
  let getfleshrename () =
    let temp = !renamecounter in
    renamecounter := temp+1;temp in
  (* ref of list of existing tyvars *)
  let tyvarlist = ref [] in
  (* ty -> subst *)
  let rec makesubst ty0 = match ty0 with
    | TyVar tyvar
      -> if List.mem tyvar !tyvarlist
          (* if tyvar exists, do nothing *)
          then Subst.empty
          (* if tyvar not exists, make subst *)
          else Subst.singleton tyvar (TyVar (getfleshrename()))
    | TyCon _ -> Subst.empty
    | TyArr (ty1, ty2) -> merge_subst (makesubst ty1) (makesubst ty2)
  in apply_subst_to_type (makesubst ty) ty

(* ref int *)
let reccount = ref 0
(* int -> unit *)
let setreccount k = reccount := k;()

exception CannotInferType1
exception CannotInferType2

(* envD -> expression -> (condtree, rules) *)
(* condtree = Node of cond * (cond tree) list *)
let rec make_condtree_and_rules envD exp = 
    match exp with
    (* var & var-P *)        
    | ExpVar x -> 
      if is_in_domD x envD
        (* var: base case *)
        (* D, x:u |- x:<;u> *)
              (* find "x:u" in D *)
        then  let typing = EnvD.find x envD in
              (* "no condition" => D,x:u|-x:<;u> *)
              let tree = Node((envD, exp, typing), []) in
              (tree, [])
        (* var-P: base case *)
        (* |-x:<x:u;u> *)
        else  let u = TyVar (getfleshtyvar()) in
              (* "no condition" => D|-x:<x:u;u> *)
              let tree = Node((envD, exp, (EnvU.singleton x u, u)), []) in
              (tree, [])
    (* con: base case *)
    (* |-c:<;type(c)> *)
    | ExpCon c -> 
      (* "no condition" => D|-c:<;type(c)> *)
      let tree = Node ((envD, exp, (EnvU.empty, consttype_of c)), []) in
      (tree, [])
    (* abs, abs-vac *)
    | ExpAbs (x, exp1) ->
      (* |-e:<U,x:u0; u1> => *)
      let (tree1, rules) = make_condtree_and_rules envD exp1 in
      let Node((envD1, _, (envU1, ty1)), _) = tree1 in
      if is_fv_in_exp x exp1 
        (* abs *)
        (* |-e:<U,x:u0; u1> => |-\x.e:<U;u0->u1> *)
        then if not(is_in_D x envD)
              then  let u = EnvU.find x envU1 in
                    (* |-e:<U,x:u0; u1> => |-\x.e:<U;u0->u1> *)
                    let tree = Node((envD1, exp, ((EnvU.remove x envU1), TyArr (u, ty1))), [tree1]) in
                    (tree, rules)
              else raise CannotInferType1
        (* abs-vac *)
        (* |-e:<U;u1> => |-\x.e:<U;U0->u1>, u0=flesh *)
        else if not(is_in_D x envD) 
              then  let u = TyVar (getfleshtyvar ()) in
                    (* |-e:<U;u1> => |-\x.e:<U;U0->u1>, u0=flesh *)
                    let tree = Node((envD1, exp, (envU1, TyArr (u, ty1))), [tree1]) in
                    (tree, rules)
              else raise CannotInferType2
    (* app *)
    (* |-e1<U1;u1>, |-e2<U2;u2> => |-e1 e2:<U1+U2;u3>, u1=u2->u3, u3=flesh *)
    | ExpApp (exp1, exp2) ->
      (* |-e1<U1;u1> *)
      let (tree1, rules1) = make_condtree_and_rules envD exp1 in
      let Node((envD1, _, (envU1, ty1)), _) = tree1 in
      (* |-e2<U2;u2> *)
      let (tree2, rules2) = make_condtree_and_rules envD exp2 in
      let Node((envD2, _, (envU2, ty2)), _) = tree2 in
        (* prepare new tyvar and type (exp1 exp2) as newtyvar.  *)
        (* ex) exp1:ty1, exp2:ty2 => exp1 exp2: ty. [ty1=ty2->ty] *)
        (* if typing assumptions corrided, the first one adapted. *)
        (* ex) x:t1 in U1, x:t2 in U2 =>  x:t1 in U1U2 [t1=t2]*)
        let tyvar = TyVar (getfleshtyvar ()) in
        let rules12 = (rules_of_samekey_in envU2 envU1) in
          (* |-e1<U1;u1>, |-e2<U2;u2> => |-e1 e2:<U1+U2;u3> *)
          let tree = Node((merge_envD envD1 envD2, exp, (merge_envU envU1 envU2, tyvar)), [tree1;tree2]) in
          (* rules1, rules2, u1=u2->u3, rules from samekey in U1+U2 *)
          let rules = rules1@rules2@[(ty1, TyArr(ty2, tyvar))]@rules12 in
          (tree, rules)
    (* rec-k *)
    | ExpRec (expvar1, exp2) -> 
      (* int -> envD -> expvar -> typing -> exp -> (cond tree list) *)
      let rec loop k envD expvar typing0 exp = 
        ( if k = 0 
            (* rec-p *)
            (* D,x:u|-rec x=e:u *)
            then
              let cond = (envD, exp, typing0) in
              let tree = Node(cond, []) in
                [tree]
            (* rec-k k>=1 *)
            else
              (* make condtree *)
              (* [tree1; tree2; tree3; ...] *)
              let (tree1, rules1) = make_condtree_and_rules (EnvD.add expvar typing0 envD) exp in
              let Node((_, _, typing1'), _) = tree1 in
                let subst = unify rules1 in
                let typing1 = apply_subst_to_typing subst typing1' in
                  (* [tree1; tree2; ...] *)
                  tree1::(loop (k-1) envD expvar typing1 exp)
        ) in
      (* condtree list -> condtree *)
      let last_of list = List.hd (List.rev list) in
        (* loop: ...-> (cond tree) list <= [tree1; tree2; ...] *)
        (* what we want is last elmnt of the list *)
        (* return Node(lst elm, list) *)
        let typing0 = ((envU_of_Bx (fvd_in_exp exp envD)), TyVar (getfleshtyvar ())) in
        let condtrees = loop !reccount envD expvar1 typing0 exp2 in
        let Node((_, _, typingk), _) = last_of condtrees in
          let tree = Node((envD, exp, typingk), condtrees) in
          (tree, [])
    | _ -> (Node((EnvD.empty, exp, (EnvU.empty, TyCon TyUnit)), []), []) (* todo:let *)

(* envD -> exp -> condtree, rules, cond *)
let infertype envD exp =
  resetcounter();
    let (tree, rules) = make_condtree_and_rules envD exp in
    let Node((envD1, _, typing), _) = tree in
      let subst = unify rules in
      (tree, rules, (apply_subst_to_envD subst envD, exp, apply_subst_to_typing subst typing))