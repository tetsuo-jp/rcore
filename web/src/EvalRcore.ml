open AbsRcore
open PrintRcore
open List

type store = (rIdent * valT) list

let vtrue = VCons (VNil, VNil)
let vfalse = VNil

let prtStore (i : int) (e : (rIdent * valT) list) : doc = 
  let rec f = function
    | [] -> concatD []
    | [(x,v)] -> concatD [prtRIdent 0 x; render ":="; prtValT 0 v]
    | (x,v) :: ss -> concatD [prtRIdent 0 x; render ":="; prtValT 0 v; render "," ; f ss]
  in concatD [render "{"; f e; render "}"]

(* リストに要素を追加する。ただし、すでにその要素がリストにある場合は追加しない。 *)
let rec insert x = function 
  | [] -> [x]
  | y :: ys -> y :: if x = y then ys else insert x ys

let merge xs ys = fold_right insert xs ys

(* Reversible update *)
let rec rupdate (x, vx) = function
  | [] -> failwith ("Variable " ^ printTree prtRIdent x ^ " is not found (1)")
  | (y, vy) :: ys -> if x = y 
		     then (if vy = VNil 
			   then (y, vx)
			   else if vx = vy 
			   then (y, VNil)
			   else failwith "error in update") :: ys
		     else (y, vy) :: rupdate (x, vx) ys

(* Irreversible update *)
let rec update (x, vx) = function
  | [] -> failwith ("Variable " ^ printTree prtRIdent x ^ " is not found (2)")
  | (y, vy) :: ys -> if x = y
		     then (y, vx) :: ys
		     else (y, vy) :: update (x, vx) ys

let all_cleared (s : store) = for_all (fun (_, v) -> v = VNil) s

(* プログラム中に使用されている変数名を列挙する。 *)
let rec varProgram (Prog (ms, x, c, y)) = insert x (insert y (varCom c))

and varMac (Mac (_,xs,c)) = merge xs (varCom c)

and varCom = function
  | CMac (_, xs) -> merge xs []
  | CAsn (x,e) -> insert x (varExp e)
  | CSeq (c, d) -> merge (varCom c) (varCom d)
  | CLoop (e, loopBranch, f) ->
     fold_right merge [varExp e; varExp f; varLoopBranch loopBranch] []
  | CShow _ -> []

and varLoopBranch = function
  | BLoop com -> varCom com
  | BLoopNone -> []

and varExp : exp -> rIdent list = function
  | ECons (e1, e2) -> merge (varExp e1) (varExp e2)
  | EHd e -> varExp e
  | ETl e -> varExp e
  | EEq (e1, e2) -> merge (varExp e1) (varExp e2)
  | EVar (Var x) -> [x]
  | EVal v -> []

(* Evaluation *)
let evalVariable s (Var x) = assoc x s

let rec evalExp s = function
    ECons (e1, e2) -> VCons (evalExp s e1, evalExp s e2)
  | EHd e -> (match evalExp s e with
	      | VNil | VAtom _ as v -> failwith ("No head. Expression " ^ printTree prtExp (EHd e) ^ " has value " ^ printTree prtValT v)
	      | VCons (v,_) -> v)
  | ETl e -> (match evalExp s e with
	      | VNil | VAtom _ as v -> failwith ("No tail. Expression " ^ printTree prtExp (ETl e) ^ " has value " ^ printTree prtValT v)
	      | VCons (_,v) -> v)
  | EEq (e1, e2) -> if evalExp s e1 = evalExp s e2 then vtrue else vfalse
  | EVar x -> evalVariable s x
  | EVal v -> v

and evalCom (s : store) : com -> store = function
  | CMac (_, _) -> failwith "Impossible happened.  Macro must not appear in runtime."
  | CAsn (y, e) -> let v' = evalExp s e in
		   rupdate (y, v') s
  | CSeq (c, d) -> let s1 = evalCom s c in
		   evalCom s1 d
  | CLoop (e, loopbranch, f) ->
     if evalExp s e = vtrue then
       evalLoop s (e, loopbranch, f)
     else failwith ("Assertion " ^ printTree prtExp e ^ " is not true.\n")
  | CShow e -> (print_string (printTree prtExp e ^ " = " ^ printTree prtValT (evalExp s e) ^ "\n"); s)

and evalLoop (s : store) (e, loopbranch, f) : store =
  if evalExp s f = vtrue
  then s
  else
    let s1 = (match loopbranch with
	 	BLoop c   -> evalCom s c
	      | BLoopNone -> s)
    in
    assert (evalExp s1 e = vfalse);
    evalLoop s1 (e, loopbranch, f)

and evalProgram (p : program) (v : valT): valT =
  let Prog (ms, x, c, y) as p' = MacroRcore.expMacProgram p in
  let s = map (fun x -> (x, VNil)) (varProgram p') in
  let s1 = rupdate (x, v) s in
  let s2 = evalCom s1 c in
  let res = evalVariable s2 (Var y) in
  let s3 = rupdate (y, res) s2 in
  if all_cleared s3 then res 
  else failwith "Some variables are not nil."
