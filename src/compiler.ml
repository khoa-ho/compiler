let is_lexing = ref false
let is_parsing = ref false
let is_stepping = ref false
let is_typechecking = ref false
let files = ref []

let cli () =
  let speclist = 
    [("-lex", Arg.Set is_lexing, "Enables lexing mode");
     ("-parse", Arg.Set is_parsing, "Enables parsing mode");
     ("-type", Arg.Set is_typechecking, "Enables typechecking mode");
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
    List.iter parse_mode ast_list

open Lang
let main () =
  let _ = cli () in
  List.iter compile (List.rev !files)

let _ = if !Sys.interactive then () else main ()
