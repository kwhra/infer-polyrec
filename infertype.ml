open Syntax
open EnvU
open EnvD
open Subst
open Variable
open Print
open Print_latex
open Basic_op
open Unify
open Inferlog

exception NotDefined

(* type(c) : expcon -> ty *)
let consttype_of c = match c with
  | Unit -> TyUnit
  | Bool -> TyBool
  | Ifc -> let u = TyVar (getfleshtyvar()) in
              TyArr(TyBool, TyArr(u, TyArr(u, u)))
  | Arth -> let u = TyVar (getfleshtyvar()) in
              TyArr(u, TyArr(u, u))

(* expvar list -> envU *)
let envU_of_Bx ls = List.fold_right 
                      (fun expvar envU -> EnvU.add expvar (TyVar (getfleshtyvar())) envU)
                      ls
                      EnvU.empty

(* ty -> ty *)
let rec rename ty = 
  let renamecounter = ref 0 in
  let getfleshrename () =
    let temp = !renamecounter in
    renamecounter := temp+1;temp in
(* doing *)
  ()
  
  

(* ref int *)
let reccount = ref 0
(* int -> unit *)
let setreccount k = reccount := k;()

exception CannotInferType1
exception CannotInferType2

(* expression -> (envD, typing, rules) *)
(* write inferlog string. (not flush) *)
let rec infertype_with_rules envD exp = 
  (* tempval for result *)
  let resenvD = ref EnvD.empty in
  let restyping = ref (EnvU.empty, TyBool) in
  let resrules = ref [] in
  (* infer type body *)
  (* return unit in each blanch*)
  (
    match exp with
    (* var & var-P *)        
    | ExpVar x -> if is_in_domD x envD
                  (* var: base case *)
                  (* D, x:u |- x:<;u> *)
                  then let typing0 = EnvD.find x envD in
                          resenvD := envD;
                          restyping := typing0;
                          resrules := []
                  (* var-P: base case *)
                  (* |-x:<x:u;u> *)
                  else let u = TyVar (getfleshtyvar()) in
                          resenvD := envD;
                          restyping := (EnvU.add x u EnvU.empty, u);
                          resrules := []
    (* con: base case *)
    (* |-c:<;type(c)> *)
    | ExpCon c -> 
      resenvD := envD;
      restyping := (EnvU.empty, consttype_of c);
      resrules := []
    (* abs, abs-vac *)
    | ExpAbs (x, exp1) ->
      (* |-e:<U,x:u0; u1> => *)
      (match (infertype_with_rules envD exp1) with (envD1, (envU1, ty1), rules) ->
        if is_fv_in_exp x exp1 
          (* abs *)
          (* |-e:<U,x:u0; u1> => |-\x.e:<U;u0->u1> *)
          then if not(is_in_D x envD) 
                then let u = EnvU.find x envU1 in
                        resenvD := envD1;
                        restyping := ((EnvU.remove x envU1), TyArr (u, ty1));
                        resrules := rules
                else raise CannotInferType1
          (* abs-vac *)
          (* |-e:<U;u1> => |-\x.e:<U;U0->u1>, u0=flesh *)
          else if not(is_in_D x envD) 
                then let u = TyVar (getfleshtyvar ()) in
                        resenvD := envD1;
                        restyping := (envU1, TyArr (u, ty1));
                        resrules := rules
                else raise CannotInferType2)
    (* app *)
    (* |-e1<U1;u1>, |-e2<U2;u2> => |-e1 e2:<U1+U2;u3>, u1=u2->u3, u3=flesh *)
    | ExpApp (exp1, exp2) ->
      (* |-e1<U1;u1> *)
      ( match (infertype_with_rules envD exp1) with (envD1, (envU1, ty1), rules1) -> 
      (* |-e1<U2;u2> *)
        match (infertype_with_rules envD exp2) with (envD2, (envU2, ty2), rules2) ->
          (* prepare new tyvar and type (exp1 exp2) as newtyvar.  *)
          (* ex) exp1:ty1, exp2:ty2 => exp1 exp2: ty. [ty1=ty2->ty] *)
          (* if typing assumptions corrided, the first one adapted. *)
          (* ex) x:t1 in U1, x:t2 in U2 =>  x:t1 in U1U2 [t1=t2]*)
          let tyvar = TyVar (getfleshtyvar ()) in
              let rules12 = (rules_of_samekey_in envU2 envU1) in
                resenvD := envDmerge envD1 envD2;
                restyping := (envUmerge envU1 envU2, tyvar);
                resrules := rules1@rules2@[(ty1, TyArr(ty2, tyvar))]@rules12 )
    (* rec-k *)
    | ExpRec (expvar1, exp2) -> 
      (* int -> envD -> expvar -> typing -> exp -> unit *)
      let rec loop k envD expvar typing0 exp = 
        (if k = 0 
          (* rec-p *)
          (* D,x:u|-rec x=e:u *)
          then  (resenvD := envD;
                restyping := typing0;
                resrules := []; )
          (* rec-k k>=1 *)
          else (writeinferlog ("% Rec " ^ (string_of_int k) ^ " start\n");
            let (envD1, typing1, rules1) = infertype_with_rules (EnvD.add expvar typing0 envD) exp in
              let subst = unify rules1 in
                let typing1' = typingsubst subst typing1 in
                  genunifylog rules1 subst;
                  writeinferlog ("% Rec " ^ (string_of_int k) ^ "end\n");
                  loop (k-1) envD expvar typing1' exp )
        ) in
      let typing0 = ((envU_of_Bx (fvd_in_exp exp envD)), TyVar (getfleshtyvar ())) in
        loop !reccount envD expvar1 typing0 exp2
    | ExpIf (exp1, exp2) -> 
      (* |-e1<U1;u1> *)
      ( match (infertype_with_rules envD exp1) with (envD1, (envU1, ty1), rules1) -> 
      (* |-e1<U2;u2> *)
        match (infertype_with_rules envD exp2) with (envD2, (envU2, ty2), rules2) ->
          (* prepare new tyvar and type (exp1 exp2) as newtyvar.  *)
          (* ex) exp1:ty1, exp2:ty2 => exp1 exp2: ty. [ty1=ty2->ty] *)
          (* if typing assumptions corrided, the first one adapted. *)
          (* ex) x:t1 in U1, x:t2 in U2 =>  x:t1 in U1U2 [t1=t2]*)
          let tyvar = TyVar (getfleshtyvar ()) in
          let rules12 = (rules_of_samekey_in envU2 envU1) in
            resenvD := envDmerge envD1 envD2;
            restyping := (envUmerge envU1 envU2, tyvar);
            resrules := rules1@rules2@[(tyvar, ty1);(tyvar, ty2)]@rules12 )
  );
  (* make log *)
  geninferlog !resenvD exp !restyping !resrules;
  (* return (envD, typing, rules) *)
  (!resenvD, !restyping, !resrules)

(* envD -> exp -> cond *)
let infertype envD exp = 
  resetcounter(); 
  resetlog();
  match (infertype_with_rules envD exp) with (envD1, typing, rules)
    -> let subst = unify rules in
        (envD, exp, typingsubst subst typing)