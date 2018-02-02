(* exemple.ml *)

let length_flag = ref false

(* let choose_mode = 
  if flag then (
    fun arg -> print_endline (string_of_int (String.length arg))
  ) else (
    fun arg -> print_endline arg
  ) *)

let main () =
  let speclist = [("-l", Arg.Set length_flag, "Enables length-printing mode")] in 
  let usage_msg = "Use -help for the list of options" in
  let flag = !length_flag in 
  if flag then (
    Arg.parse speclist (fun arg -> print_endline (string_of_int (String.length arg))) usage_msg
  ) else (
    Arg.parse speclist (fun arg -> print_endline arg) usage_msg
  )

let _ = main ()