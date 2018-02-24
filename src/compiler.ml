let is_lexing = ref false
let is_parsing = ref false
let is_stepping = ref false
let files = ref []

let cli () =
  let speclist = 
    [("-lex", Arg.Set is_lexing, "Enables lexing mode");
     ("-parse", Arg.Set is_parsing, "Enables parsing mode");
     ("-step", Arg.Set is_stepping, "Enables small-step evaluation mode")] 
  in 
  let usage_msg = "Usage: ./compiler.native [flags] [source_paths]\nAvailable flags:" in
  Arg.parse speclist (fun filename -> files :=  filename :: !files) usage_msg

let compile filename =
  let lexbuf = (Lexing.from_channel (open_in filename)) in
  let _ = Lexer.curr_file lexbuf filename in
  if !is_lexing then
    let rec lexing toks =
      let token = Lexer.lex lexbuf in
      match token with
      | Parser.EOF -> toks |> Lexer.string_of_token_list |> print_endline
      | _ -> lexing (token :: toks)
    in lexing []
  else 
    let ast_list = Parser.parse Lexer.lex lexbuf in
    if !is_parsing then
      List.map Lang.string_of_exp ast_list |> List.iter print_endline
    else if !is_stepping then
      List.iter Lang.step_interpret ast_list
    else
      let interpret ast =
        Lang.typecheck Lang.Context.empty ast |> ignore;
        Lang.interpret ast |> Lang.string_of_exp |> print_endline
      in
      List.iter interpret ast_list

open Lang
let main () =
  let _ = cli () in
  List.iter compile (List.rev !files)
(* Lang.typecheck Context.empty (EFunc ("x", TypInt, TypBool, EBop (OLeq, EVar "x", EFloat 5.6))) |> Lang.string_of_typ |> print_endline *)

let _ = if !Sys.interactive then () else main ()
