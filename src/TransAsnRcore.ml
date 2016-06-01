open AbsRcore

let t = ref 10

let fresh = t := !t + 1; "X" ^ string_of_int (!t)

let rec transAsn (x : rIdent) (e : exp) : com =
  let y = RIdent fresh in
  CAsn (x, EVar (Var y))
