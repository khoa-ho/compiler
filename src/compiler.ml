let is_lexing = ref false
let is_parsing = ref false
let files = ref []

let cli () =
  let speclist = 
    [("-lex", Arg.Set is_lexing, "Enables lexing mode");
     ("-parse", Arg.Set is_parsing, "Enable parsing mode");] 
  in 
  let usage_msg = "Usage: ./compiler.native [flags] [args]\nAvailable flags:" in
  Arg.parse speclist (fun filename -> files :=  filename :: !files) usage_msg

let compile filename =
  let tokens   = Lexer.lex (Stream.of_channel (open_in filename)) in
  if !is_lexing then
    tokens |> Lexer.string_of_token_list |> print_endline
  else
    let (e, _)   = Parser.parse tokens in
    if !is_parsing then
      Lang.string_of_exp e |> print_endline
    else
      Lang.interpret e |> Lang.string_of_exp |> print_endline

let main () =
  let _ = cli () in
  List.iter compile !files

let _ = if !Sys.interactive then () else main ()
