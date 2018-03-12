let is_lexing = ref false
let is_parsing = ref false
let is_stepping = ref false
let is_typechecking = ref false
let is_in_repl = ref false
let files = ref []

let cli () =
  let speclist = [
    ("-lex", Arg.Set is_lexing, "Enables lexing mode");
    ("-parse", Arg.Set is_parsing, "Enables parsing mode");
    ("-step", Arg.Set is_stepping, "Enables small-step evaluation mode");
    ("-type", Arg.Set is_typechecking, "Enables typechecking mode");
    ("-repl", Arg.Set is_in_repl, "Enables repl mode")
  ]
  in 
  let usage_msg = "Usage: ./compiler.native [flags] [source_paths]\nAvailable flags:" in
  Arg.parse speclist (fun filename -> files :=  filename :: !files) usage_msg

let compile filename =
  let lexbuf = Lexing.from_channel (open_in filename) in
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
    let parse_mode ast =
      if !is_parsing then
        Lang.string_of_exp Lang.Environ.empty ast |> print_endline
      else if !is_typechecking then
        Lang.type_check ast |> Lang.string_of_typ |> print_endline
      else if !is_stepping then
        begin
          Lang.type_check ast |> ignore;
          Lang.interpret_step ast
        end
      else
        begin
          Lang.type_check ast |> ignore;
          let state = Lang.interpret ast in
          Lang.string_of_exp (Lang.fst state) (Lang.snd state) |> print_endline
        end
    in
    try List.iter parse_mode ast_list
    with Lang.TypeError -> exit 1

let rec run_repl () =
  print_string "# ";
  let input = read_line () in
  if input = "quit" then ()
  else
    let interpret ast =
      let typ = Lang.type_check ast in
      let state = Lang.interpret ast in
      Lang.string_of_exp (Lang.fst state) (Lang.snd state) 
      |> Printf.sprintf "- : %s = %s" (Lang.string_of_typ typ)
      |> print_endline
    in
    try
      input
      |> Lexing.from_string 
      |> Parser.parse Lexer.lex
      |> List.iter interpret
    with
    | Lang.TypeError -> run_repl ()
    | Lexer.SyntaxError msg -> 
      print_endline ("Error: " ^ msg); 
      run_repl () 
    | _ -> 
      print_endline "Error: Parsing error"; 
      run_repl ()

let main () =
  let _ = cli () in
  if !is_in_repl then
    run_repl () 
  else
    List.iter compile (List.rev !files)

let _ = if !Sys.interactive then () else main ()
