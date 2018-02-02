let print_length = function
  arg -> print_endline (string_of_int (String.length arg))

let print_arg = function
  arg -> print_endline arg

let main () =
  let speclist = [("-length", Arg.Rest print_length, "Prints the lengths of each of the arguments")] in 
  let usage_msg = "Usage: my-project [flags] [args]\nAvailable flags:" in
  Arg.parse speclist (fun arg -> print_endline arg) usage_msg

let _ = main ()