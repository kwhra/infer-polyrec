(* definitionss of type *)

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
  | ExpIf of expression * expression
  | ExpLet of expvar * expression * expression

type tyvar = int (* 0=A, 1=B, 2=C, ... *)
type ty = 
  | TyUnit
  | TyBool
  | TyVar of tyvar
  | TyArr of ty * ty

(* like (expvar * ty) list *)
module EnvU = Map.Make(struct type t = expvar let compare = compare end)

type typing = (ty EnvU.t) * ty

(* like (tyvar * ty) list *)
module Subst = Map.Make(struct type t = tyvar let compare = compare end)

type rules = (ty * ty) list

(* new: *)
(* like (expvar * typing) list *)
module EnvD = Map.Make(struct type t = expvar let compare = compare end)

type condition = ((ty EnvD.t) * expression * typing)
