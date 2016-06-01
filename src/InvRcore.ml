open AbsRcore
open String

(* 「INV-マクロ名」と「マクロ名」は逆命令になる。
 * invMacroName は、「マクロ名」を「INV-マクロ名」に、「INV-マクロ名」を「マクロ名」に書き換える。 *)
let invMacroName (RIdent str) : rIdent = 
  RIdent (if length str >= 5 && sub str 0 4 = "INV-"
	  then sub str 4 (length str - 4)
	  else "INV-" ^ str)

let rec invCom = function
    CMac (x, idents) -> CMac (invMacroName x, idents) 
  | CAsn (x, e) -> CAsn (x, e)
  | CSeq (c, d) -> CSeq (invCom d, invCom c)
  | CLoop (e, loopbranch, f) ->
     CLoop (f, invLoopBranch loopbranch, e)
  | CShow e -> CShow e

and invLoopBranch = function
    BLoop c   -> BLoop (invCom c)
  | BLoopNone -> BLoopNone

let invProgram (Prog (macros, x, c, y)) = Prog (macros, y, invCom c, x)
