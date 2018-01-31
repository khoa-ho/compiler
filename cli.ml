let verbose = ref false
let max_files_to_list = ref 42
let dir_to_list = ref "."
 
let set_max_files nbr_of_files = max_files_to_list := nbr_of_files
 
let set_directory dir = dir_to_list := dir
 
let main =
begin
let speclist = [("-v", Arg.Set verbose, "Enables verbose mode");
("-n", Arg.Int (set_max_files), "Sets maximum number of files to list");
("-d", Arg.String (set_directory), "Names directory to list files");
]
in let usage_msg = "MyLs2000 is a revolutionary file listing tool. Options available:"
in Arg.parse speclist print_endline usage_msg;
print_endline ("Verbose mode: " ^ string_of_bool !verbose);
print_endline ("Max files to list: " ^ string_of_int !max_files_to_list);
print_endline ("Directory to list files: " ^ !dir_to_list);
end
 
let () = main