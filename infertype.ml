open Syntax
open EnvU
open EnvD
open Subst
open Variable
open Print
open Print_latex
open Baseop
open Rename
open Unify
open Inferlog

(* type(c) : expcon -> ty *)
let consttype_of c = match c with
  | Unit -> TyCon TyUnit
  | Bool -> TyCon TyBool
  | Ifc -> let u = TyVar (get_fleshtyvar()) in
              TyArr(TyCon TyBool, TyArr(u, TyArr(u, u)))
  | Arth -> let u = TyVar (get_fleshtyvar()) in
              TyArr(u, TyArr(u, u))

(* expvar list -> typing *)
let typingBx ls = 
  let envU = 
  List.fold_right 
    (fun expvar envU -> EnvU.add expvar (TyVar (get_fleshtyvar())) envU)
    ls
    EnvU.empty
  in
  (envU, (TyVar (get_fleshtyvar())))

(* ref int *)
let reccount = ref 0
(* int -> unit *)
let setreccount k = reccount := k;()

(* typing -> typing -> bool *)
let is_equal_typing typing1 typing2 = 
	let new1 = rename_typing typing1 in
	let new2 = rename_typing typing2 in
	new1 = new2

exception ErrorInfertype1
exception ErrorInfertype2
exception ErrorInfertype3
exception CannotInferTyping

(* envD -> expression -> condrulestree *)
(* condrulestree = Node ( (cond, rules), (condrulestree) list) *)
let rec make_condrulestree envD exp = 
  match exp with
  (* var & var-P *)        
  | ExpVar x -> 
    if is_in_domD x envD
      (* var: base case *)
      (* D, x:u |- x:<;u> *)
            (* find "x:u" in D *)
      then  let typing = EnvD.find x envD in
            (* "no condition" => D,x:u|-x:<;u> *)
            let condrule = ((envD, exp, typing), []) in
              Node (condrule, [])
      (* var-P: base case *)
      (* |-x:<x:u;u> *)
      else  let u = TyVar (get_fleshtyvar()) in
            (* "no condition" => D|-x:<x:u;u> *)
            let condrule = ((envD, exp, (EnvU.singleton x u, u)), []) in
              Node (condrule, [])
  (* con: base case *)
  (* |-c:<;type(c)> *)
  | ExpCon c -> 
    (* "no condition" => D|-c:<;type(c)> *)
    let condrule = ((envD, exp, (EnvU.empty, consttype_of c)), []) in
      Node (condrule, [])
  (* abs, abs-vac *)
  | ExpAbs (x, exp1) ->
    (* |-e:<U,x:u0; u1> => *)
    let tree1 = make_condrulestree envD exp1 in
    let Node ((cond1, rules1), _) = tree1 in
    let (envD1, _, (envU1, ty1)) = cond1 in
    if is_fv_in_exp x exp1 
      (* abs *)
      (* |-e:<U,x:u0; u1> => |-\x.e:<U;u0->u1> *)
      then if not(is_in_D x envD)
            then  let u = EnvU.find x envU1 in
                  (* |-e:<U,x:u0; u1> => |-\x.e:<U;u0->u1> *)
                  let cond = (envD1, exp, ((EnvU.remove x envU1), TyArr (u, ty1))) in
                    Node ((cond, rules1), [tree1])
            else raise ErrorInfertype1
      (* abs-vac *)
      (* |-e:<U;u1> => |-\x.e:<U;U0->u1>, u0=flesh *)
      else if not(is_in_D x envD) 
            then  let u = TyVar (get_fleshtyvar ()) in
                  (* |-e:<U;u1> => |-\x.e:<U;U0->u1>, u0=flesh *)
                  let cond = (envD1, exp, (envU1, TyArr (u, ty1))) in
                    Node ((cond, rules1), [tree1])
            else raise ErrorInfertype2
  (* app *)
  (* |-e1<U1;u1>, |-e2<U2;u2> => |-e1 e2:<U1+U2;u3>, u1=u2->u3, u3=flesh *)
  | ExpApp (exp1, exp2) ->
    (* |-e1<U1;u1> *)
    let tree1 = make_condrulestree envD exp1 in
    let Node((cond1, rules1), _) = tree1 in
    let (envD1, _, (envU1, ty1)) = cond1 in
    (* |-e2<U2;u2> *)
    let tree2 = make_condrulestree envD exp2 in
    let Node((cond2, rules2), _) = tree2 in
    let (envD2, _, (envU2, ty2)) = cond2 in
      (* prepare new tyvar and type (exp1 exp2) as newtyvar.  *)
      (* ex) exp1:ty1, exp2:ty2 => exp1 exp2: ty. [ty1=ty2->ty] *)
      (* if typing assumptions corrided, the first one adapted. *)
      (* ex) x:t1 in U1, x:t2 in U2 =>  x:t1 in U1U2 [t1=t2]*)
      let tyvar = TyVar (get_fleshtyvar ()) in
      let rules12 = (rules_of_samekey_in envU2 envU1) in
        (* |-e1<U1;u1>, |-e2<U2;u2> => |-e1 e2:<U1+U2;u3> *)
        let cond = (merge_envD envD1 envD2, exp, (merge_envU envU1 envU2, tyvar)) in
        (* rules1, rules2, u1=u2->u3, rules from samekey in U1+U2 *)
        let rules = rules1@rules2@[(ty1, TyArr(ty2, tyvar))]@rules12 in
          Node((cond, rules), [tree1;tree2])
  (* rec-k *)
  | ExpRec (expvar1, exp2) -> 
    (* int -> envD -> expvar -> typing -> exp -> (condrules tree list) *)
    let rec loop k envD expvar typing0 exp = 
      ( if k = 0
          (* rec-p *)
          (* D,x:u|-rec x=e:u *)
          then []
          (* rec-k k>=1 *)
          else
            (* make condtree *)
            (* [tree1; tree2; tree3; ...] *)
            let tree1' = make_condrulestree (EnvD.add expvar typing0 envD) exp in
            let Node(((_, _, typing1'), rules1'), _) = tree1' in
              (* unify rules and apply to tree1' *)
              let subst = unify rules1' in
              let typing1 = apply_subst_to_typing subst typing1' in
                (* [tree1; tree2; ...] *)
                (* add tree before subst. *)
                tree1'::(loop (k-1) envD expvar typing1 exp)
      )
    in
    (* condruletree list -> condruletree *)
    let last_of list = List.hd (List.rev list) in
    let second_last_of list = List.hd (List.tl (List.rev list)) in
    (* Rec-ML *)
    (* envD -> expvar -> exp -> condrulestree *)
    (* D|-e:<U,x:u; u> => D|-rec{x=e}:<U;u> *)
    let recML d x e = 
      let condtree = make_condrulestree d e in
      let Node(((_, _, (envU', ty')), rules), _) = condtree in
      let rule = (EnvU.find x envU', ty') in
      let subst = unify (rule::rules) in
      let (envU, ty) = apply_subst_to_typing subst (envU', ty') in
      let cond = (d, ExpRec(x, e), (EnvU.remove x envU, ty)) in
          Node ((cond, []), [condtree])
    in
      (* store current tyvar *)
      (* load after "ExpRec" *)
      let typing0 = typingBx (fvd_in_exp exp envD) in
      let condtrees = loop !reccount envD expvar1 typing0 exp2 in
      (* loop: ...-> (cond tree) list = [tree1; tree2; ...] *)
      (* (typingk, k-1) is  (last, second-last) element of list *)
      (* if typingk = typingk-1 then able to infer type *)
        let treek = last_of condtrees in
        let Node (((_, _, typingk'), rulesk), _) = treek in
        let substk = unify rulesk in
        let typingk = apply_subst_to_typing substk typingk' in
          (match condtrees with
          | [] -> raise ErrorInfertype3
          (* if k = 1 *)
          | [tree1] -> 
            (* if typing0 = typing1 then able to infer type *)
            if is_equal_typing typing0 typingk
              then  let cond = (envD, exp, typingk) in
                    Node((cond, []), condtrees)
              (* Rec-ML *)
              else recML envD expvar1 exp2 
          (* otherwise: k>=2 *)
          | _ -> 
            (* denote typingk-1 as typingkk *)
            let treekk = second_last_of condtrees in
            let Node (((_, _, typingkk'), ruleskk), _) = treekk in
            let substkk = unify ruleskk in
            let typingkk = apply_subst_to_typing substkk typingkk' in
              if is_equal_typing typingk typingkk
                then  let cond = (envD, exp, typingk) in
                      Node((cond, []), condtrees) 
                else recML envD expvar1 exp2 )
  (* Let *)
  (* D|-e2:<U2;u2>, D|-e3:<U3,x:u;u3> (u2 = u)*)
  (* => D|-let x=e2 in e3:<U;u3> *)
  | ExpLet (expvar, exp2, exp3) ->
    (* |-e2<U2;u2> *)
    let tree2 = make_condrulestree envD exp2 in
    let Node((cond2, rules2), _) = tree2 in
    let (envD2, _, (envU2, ty2)) = cond2 in
    (* |-e3<U3;u3> *)
    let tree3 = make_condrulestree envD exp3 in
    let Node((cond3, rules3), _) = tree3 in
    let (envD3, _, (envU3, ty3)) = cond3 in
      (* u2 = u *)
      let rule = (ty2, EnvU.find expvar envU3) in
      let cond = (merge_envD envD2 envD3, exp, (merge_envU envU2 envU3, ty3)) in
      Node ((cond, rule::(rules2@rules3)), [tree2;tree3])


(* cond rules tree -> cond tree *)
let unifiedcondtree_of condrulestree =
  (* condrulestree -> condrulestree *)
  let rec specify_rectree crtree = 
    let Node (node, childs) = condrulestree in
    let ((_, exp, _), _) = node in
    match exp with
    | ExpRec _ ->
      let newchilds = 
        List.map
          (* condrulestree -> condrulestree *)
          ( fun tree ->
              let Node (((d, e, t), r), c) = tree in
              let subst = unify r in
              apply_subst_to_condrulestree subst tree )
          childs
      in
      Node (node, newchilds)
    | _ -> 
      let newchilds = List.map specify_rectree childs in
      Node (node, newchilds)
  in
    let newtree' = specify_rectree condrulestree in
    let Node((cond, rules), _) = newtree' in
    let subst = unify rules in
    let newtree = apply_subst_to_condrulestree subst newtree' in
    removerules newtree

(* envD -> exp -> condrulestree, condtree *)
let infertype envD exp =
  reset_counter();
    let condrulestree = make_condrulestree envD exp in
      (removerules condrulestree, unifiedcondtree_of condrulestree)