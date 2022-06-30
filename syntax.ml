(* definitionss of expression (term, form) *)
(* exp = var, const, abstraction, application, rec, let *)
type expvar = string

type expcon = 
  | Unit
  | Bool
  | Ifc
  | Arth

type expression =
  | ExpVar of expvar
  | ExpCon of expcon
  | ExpAbs of expvar * expression
  | ExpApp of expression * expression
  | ExpRec of expvar * expression
  | ExpLet of expvar * expression * expression

(* definitions of type *)
(* type = const, var, unit, arrow *)
type tyvar = int 

type tycon = 
  | TyBool
  | TyUnit
  
type ty = 
  | TyCon of tycon
  | TyVar of tyvar
  | TyArr of ty * ty

(* (expvar * ty) list *)
module EnvU = Map.Make(struct type t = expvar let compare = compare end)

type typing = (ty EnvU.t) * ty

(* (tyvar * ty) list *)
module Subst = Map.Make(struct type t = tyvar let compare = compare end)

type rules = (ty * ty) list

(* (expvar * typing) list *)
module EnvD = Map.Make(struct type t = expvar let compare = compare end)

(* envD, expression, typing *)
type condition = ((ty EnvD.t) * expression * typing)

type 'a tree = Node of 'a * ('a tree list)

(* condition tree *)
(* (D, exp, ty) -- (D, exp, ty) *)
(*              |- (D, exp, ty) *)                 
type condtree = condition tree

(* (condition, rules) tree *)
(* (D, exp, ty),[rules] -  (D, exp, ty),[rules] *)
(*                 |- (D, exp, ty),[rules] *) 
type condruletree = (condition * rules) tree

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