(* Name: <SongZihao> *)
(* Subject: CS 225 Spring 2018 - Final Project *)
(* Subtyping *)

open Util
open StringSetMap

(* The Assignment:
 * 
 * implementation of Subtyping
 *)

exception NOT_FOUND


(* Expressions.
 *
 * e ∈ exp ⩴ true | false | if(e){e}{e}
 *         | ⟨e,e⟩ | projl(e) | projr(e)
 *         | ref(e) | !e | e ≔ e | e ; e
 *         | loc(ℓ)
 *)
type exp =
  | True
  | False
  | If of exp * exp * exp
  | Pair of exp * exp
  | Projl of exp
  | Projr of exp
  | Ref of exp
  | Deref of exp
  | Assign of exp * exp
  | Sequence of exp * exp
  | Loc of loc
[@@deriving show {with_path = false}]

(* Values.
 * v ∈ value ⩴ Top
 *           | T->t
 *)
type value =
  raise TODO
[@@deriving show {with_path = false}]

(*
*   Γ ⊢ t1:S  S<:T
---------------------
	 Γ ⊢ t:T
*
*)

type set_subtype = raise TOD


let rec step (e0 : exp) (t : type) : result = match e0 with
  | True -> Val(VTrue)
  | False -> Val(VFalse)
  | If(e1,e2,e3) -> begin match step e1 s with
      | Val(VTrue) -> Step(e2,s)
      | Val(VFalse) -> Step(e3,s)
      | Val(_) -> Stuck
      | Step(e1',s') -> Step(If(e1',e2,e3),s')
      | Stuck -> Stuck
      end
  | Pair(e1,e2) -> begin match step e1 s with                                  
      | Val(v1) -> begin match step e2 s with                                
          | Val(v2) -> Val(VPair(v1,v2))                                       
          | Step(e2',s') -> Step(Pair(e1,e2'),s')                            
          | Stuck -> Stuck                                                     
          end                                                                
      | Step(e1',s') -> Step(Pair(e1',e2),s')                                  
      | Stuck -> Stuck                                                       
      end                                                                      
  | Projl(e1) -> begin match step e1 s with                                  
      | Val(VPair(v1,v2)) -> Step(exp_of_val v1,s)                             
      | Val(_) -> Stuck                                                      
      | Step(e1',s') -> Step(Projl(e1'),s')                                    
      | Stuck -> Stuck                                                       
      end                                                                      
  | Projr(e1) -> begin match step e1 s with                                  
      | Val(VPair(v1,v2)) -> Step(exp_of_val v2,s)                             
      | Val(_) -> Stuck                                                      
      | Step(e1',s') -> Step(Projr(e1'),s')                                    
      | Stuck -> Stuck                                                       
      end                                                                      
                                                                  
                                                                      

(* The reflexive transitive closure of the small-step semantics relation *)
let rec step_star (e : exp) (s : store) : exp * store = match step e s with
  | Val(v) -> (exp_of_val v,s)
  | Step(e',s') -> step_star e' s'
  | Stuck -> (e,s)

(* Types.
 *
 * τ ∈ ty ⩴ bool
 *        | τ × τ
 *        | ref(τ)
 *)
type ty =
  | Bool
  | Prod of ty * ty
  | Ref of ty
[@@deriving show {with_path = false}]

type store_ty = (loc * ty) list
[@@deriving show {with_path = false}]

let rec store_ty_lookup (l : loc) (st : store_ty) : ty = match st with
  | [] -> raise NOT_FOUND
  | (l',t) :: st' -> if l = l' then t else store_ty_lookup l st'

exception TYPE_ERROR

let rec infer (e : exp) (st : store_ty) : ty = match e with
  | True -> Bool
  | False -> Bool
  | If(e1,e2,e3) ->
      let t1 = infer e1 st in
      let t2 = infer e2 st in
      let t3 = infer e3 st in
      if not (t1 = Bool) then raise TYPE_ERROR else
      if not (t2 = t3) then raise TYPE_ERROR else
      t2
  | Pair(e1,e2) ->
      let t1 = infer e1 st in
      let t2 = infer e2 st in
      Prod(t1,t2)
  | Projl(e1) ->
      let t1 = infer e1 st in
      begin match t1 with
      | Prod(t11,_) -> t11
      | _ -> raise TYPE_ERROR
      end
  | Projr(e1) ->
      let t1 = infer e1 st in
      begin match t1 with
      | Prod(_,t12) -> t12
      | _ -> raise TYPE_ERROR
      end
         
                                                              
(* Name: <SongZihao> *)
