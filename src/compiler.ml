let is_lexing = ref false
let is_parsing = ref false
let files = ref []

let cli () =
  let speclist = 
    [("-lex", Arg.Set is_lexing, "Enables lexing mode");
     ("-parse", Arg.Set is_parsing, "Enables parsing mode");] 
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
    let ast   = Parser.parse Lexer.lex lexbuf in
    if !is_parsing then
      List.map Lang.string_of_exp ast |> List.iter print_endline
    else
      List.map Lang.interpret ast |> List.map Lang.string_of_exp |> List.iter print_endline

let main () =
  let _ = cli () in
  List.iter compile (List.rev !files)

let _ = if !Sys.interactive then () else main ()
