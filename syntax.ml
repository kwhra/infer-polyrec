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
type tyvar = int (* 0=A, 1=B, 2=C, ... *)

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