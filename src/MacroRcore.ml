(* Macro Expansion *)

open PrintRcore
open AbsRcore
open InvRcore
open List

(* Substitution *)
type subst = (rIdent * rIdent) list

let rec substRIdent (ss : subst) (x : rIdent) : rIdent = try assoc x ss with Not_found -> x

and substVariable ss (Var x) = Var (substRIdent ss x)

and substExp ss = function
    ECons (e, f) -> ECons (substExp ss e, substExp ss f)
  | EHd e -> EHd (substExp ss e)
  | ETl e -> ETl (substExp ss e)
  | EEq (e, f) -> EEq (substExp ss e, substExp ss f)
  | EVar x -> EVar (substVariable ss x)
  | EVal v -> EVal v

and substCom ss = function
    CMac (m, xs) -> let xs' = map (fun y -> try assoc y ss with Not_found -> y) xs
		    in CMac (m, xs')
  | CAsn (x, e) -> CAsn (substRIdent ss x, substExp ss e)
  | CSeq (c, d) -> CSeq (substCom ss c, substCom ss d)
  | CLoop (e, loopbranch, f) ->
     CLoop (substExp ss e, substLoopBranch ss loopbranch, substExp ss f)
  | CShow e -> CShow (substExp ss e)

and substLoopBranch s = function
    BLoop c   -> BLoop (substCom s c)
  | BLoopNone -> BLoopNone


(* Macro expansion *)
let rec expMacCom (ms : macro list) = function
    CMac (m, xs) -> let rec expandMacro = function
		      | [] -> failwith ("Macro " ^ printTree prtRIdent m ^ " not found")
		      | Mac (m',xs',c) :: ns -> (match if invMacroName m' = m
						       then expMacCom ms (substCom (combine xs' xs) (invCom c))
						       else if m = m' 
						       then expMacCom ms (substCom (combine xs' xs) c) 
						       else expandMacro ns with
						 | exception (Invalid_argument _) -> (* "List.combine" *)
						    failwith ("Macro " ^ printTree prtRIdent m ^ " expects " ^ string_of_int (length xs') ^ " argument(s) but got " ^ string_of_int (length xs))
						 | default -> default
						)
		    in expandMacro ms
  | CAsn _ as e -> e
  | CSeq (c, d) -> CSeq (expMacCom ms c, expMacCom ms d)
  | CLoop (e, loopbranch, f) -> 
     CLoop (e, expMacLoopBranch ms loopbranch, f)
  | CShow e -> CShow e

and expMacLoopBranch ms = function
    BLoop c   -> BLoop (expMacCom ms c)
  | BLoopNone -> BLoopNone

and expMacProgram (Prog (ms, x, c, y)) = Prog ([], x, expMacCom ms c, y)
