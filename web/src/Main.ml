let parse (c : in_channel) : AbsRcore.program =
    ParRcore.pProgram LexRcore.token (Lexing.from_channel c)

let parseValT (c : in_channel) : AbsRcore.valT =
    ParRcore.pValT LexRcore.token (Lexing.from_channel c)

let showTree (t : AbsRcore.program) : string =
    PrintRcore.printTree PrintRcore.prtProgram t

let showValT (t : AbsRcore.valT) : string =
    PrintRcore.printTree PrintRcore.prtValT t

let () =
  let files = ref [] in
  let f_inv = ref false in
  let f_p2d = ref false in
  let f_exp = ref false in
  Arg.parse
    [("-inverse", Arg.Set f_inv, "inversion");
     ("-p2d", Arg.Set f_p2d, "translation from programs to data");
     ("-exp", Arg.Set f_exp, "expand macro")]
    (fun s -> files := !files @ [s])
    ("R-WHILE Interpreter (C) Tetsuo Yokoyama\n" ^
       Printf.sprintf "usage: %s [-inverse] [-p2d] program [data]" Sys.argv.(0));
  match !files with
  | [prog_filename] ->
     let channel = open_in prog_filename in
     let prog1 = parse channel in
     let _ = close_in channel in
     let prog2 = if !f_exp then MacroRcore.expMacProgram prog1 else prog1 in
     let prog3 = if !f_inv then InvRcore.invProgram prog2 else prog2 in
     print_endline (if !f_p2d
		   then showValT (Program2DataRcore.program2data prog3)
		   else showTree prog3)
  | [prog_filename; data_filename] -> 
     let channel = open_in prog_filename in
     let prog = parse channel in 
     let _ = close_in channel in
     let channel = open_in data_filename in
     let data = parseValT channel in 
     let _ = close_in channel in
     print_endline (showValT (EvalRcore.evalProgram prog data))
  | _ -> failwith "Invalid arguments"
