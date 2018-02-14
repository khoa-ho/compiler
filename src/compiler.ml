let main () =
  let filename = Sys.argv.(1) in
  let tokens   = Lexer.lex (Stream.of_channel (open_in filename)) in
  let (e, _)   = Parser.parse tokens in
  Lang.interpret e |> string_of_int |> print_endline

let _ = if !Sys.interactive then () else main ()
