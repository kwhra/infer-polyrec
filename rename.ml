(* rename ty, envU, ... *)
(* B->B => A->A, X->Y => A->B, Y->X => X->Y *)

open Syntax
open EnvU
open EnvD
open Subst
open Variable
open Baseop

exception CannotRename1
exception CannotRename2

(* all tyvar attached `false': need to rename *)
(* all tycon attached `true' : need not to *)
type type_with_flag = 
	| TyfCon of tycon * bool
  | TyfVar of tyvar * bool
  | TyfArr of type_with_flag * type_with_flag

(* ty -> tyf *)
(* all tyvar attached `false': need to rename *)
(* all tycon attached `true' : need not to *)
let rec tyf_of_type ty = 
	match ty with
	| TyCon tycon -> TyfCon (tycon, true)
	| TyVar tyvar -> TyfVar (tyvar, false)
	| TyArr (ty1, ty2) -> TyfArr (tyf_of_type ty1, tyf_of_type ty2)

(* tyf -> ty *)
let rec ty_of_tyf tyf = 
	match tyf with 
	| TyfCon (tycon, is_renamed) -> 
		if is_renamed then TyCon tycon else raise CannotRename1
	| TyfVar (tyvar, is_renamed) ->
		if is_renamed then TyVar tyvar else raise CannotRename2
	| TyfArr (tyf1, tyf2) ->
		TyArr (ty_of_tyf tyf1, ty_of_tyf tyf2)

(* subst -> tyf -> tyf *)
let rec apply_subst_to_tyf subst tyf = 
	match tyf with 
	| TyfCon (_, _) -> tyf
	| TyfVar (tyvar, is_renamed) -> 
		if is_renamed 
			then tyf
			else 
				if Subst.mem tyvar subst
					then TyfVar (tyvar_of (apply_subst_to_type subst (TyVar tyvar)), true)
					else tyf
	| TyfArr (tyf1, tyf2) ->
		TyfArr (apply_subst_to_tyf subst tyf1, apply_subst_to_tyf subst tyf2)

(* substlist -> tyf -> tyf *)
let rec apply_substlist_to_tyf substlist tyf =
	match substlist with
	| []     -> tyf
	| hd::tl -> apply_substlist_to_tyf tl (apply_subst_to_tyf hd tyf)

(* substlist -> ty -> ty *)
let apply_substlist_to_ty substlist ty = 
	let tyf' = tyf_of_type ty in
	let tyf = apply_substlist_to_tyf substlist tyf' in
	 ty_of_tyf tyf

(* envU -> substlist -> envU *)
let apply_substlist_to_envU substlist envU = 
	EnvU.map
		(fun ty -> apply_substlist_to_ty substlist ty)
		envU

(* typing -> substlist -> typing *)
let apply_substlist_to_typing substlist typing = 
	let (envU, ty) = typing in
		(apply_substlist_to_envU substlist envU, apply_substlist_to_ty substlist ty)

(* ref tyvar *)
let rename_counter = ref 0

(* unit -> tyvar *)
let get_fleshrename () = 
	let temptyvar = !rename_counter in
		rename_counter:=temptyvar+1;
		temptyvar

(* unit -> unit *)
let reset_rename () = rename_counter := 0

(* ref of list of existing tyvars *)
let tyvarlist = ref []

(* tyvar -> unit *)
let add_tyvarlist tyvar = 
	let temp = !tyvarlist in
	tyvarlist := tyvar::temp

(* unit -> unit *)
let reset_tyvarlist () = tyvarlist := []

(* ty -> subst list *)
(* rename with current flesh tyvar *)
let renamesubstlist_of_ty ty = 
	(* ty -> subst list *)
	let rec renamesubstlist_of_ty_loop ty0 = 
		match ty0 with
		| TyCon tycon -> []
		| TyVar tyvar -> 
			if List.mem tyvar !tyvarlist 
				then []
				else  (add_tyvarlist tyvar;[Subst.singleton tyvar (TyVar (get_fleshrename ()))])
		| TyArr (ty1, ty2) ->
			let list1 = renamesubstlist_of_ty_loop ty1 in
			let list2 = renamesubstlist_of_ty_loop ty2 in
			list1@list2
	in
	reset_rename();
	reset_tyvarlist();
	renamesubstlist_of_ty_loop ty

let rename_type ty = 
	let substlist = renamesubstlist_of_ty ty in
	apply_substlist_to_ty substlist ty

(* (envU, ty) -> (envU, ty) *)
let rename_typing typing = 
	let (envU, ty) = typing in
	let substlist = renamesubstlist_of_ty ty in
	(apply_substlist_to_envU substlist envU, apply_substlist_to_ty substlist ty)
