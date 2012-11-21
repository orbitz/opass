open Core.Std

module Command = Core_extended.Deprecated_command

let db_file = ref "-"
let db_file_f =
  Command.Flag.set_string "-db" db_file
    ~doc:" Database file, - (the default) for stdin"

let merge_file = ref ""
let merge_file_f =
  Command.Flag.set_string "-merge" merge_file
    ~doc:" Merge file, assumed to be 1Password"


let read_db db_file =
  match In_channel.input_all (open_in db_file) with
    | "" ->
      Db.make ()
    | s  ->
      Db.of_string s


let run_add db_file =
  ()

let run_search db_file term =
  ()

let run_merge _curr_db _src_file =
  ()

let add_cmd = Command.create_no_accum
  ~summary:"Add an entry"
  ~usage_arg:"[-db file]"
  ~flags:[db_file_f]
  ~final:(function
    | [] -> ()
    | _  -> failwith "No arguments should be specified")
  (fun () -> run_add !db_file)

let search_cmd = Command.create_no_accum
  ~summary:"Perform a search"
  ~usage_arg:"[-db file] term"
  ~flags:[db_file_f]
  ~final:(function
    | [search] -> search
    | _        -> failwith "Need search term")
  (fun search -> run_search !db_file search)

let merge_cmd = Command.create_no_accum
  ~summary:"Merge a 1Password file into current db"
  ~usage_arg:"[-db file] -merge file"
  ~flags:[db_file_f; merge_file_f]
  ~final:(function
    | [] -> ()
    | _  -> failwith "No arguments should be specified")
  (fun () -> run_merge !db_file !merge_file)

let main () =
  Exn.handle_uncaught ~exit:true (fun () ->
    Command.run ~version:"0.1" ~build_info:"N/A"
      (Command.group ~summary:"opass commands"
	 [ ("add", add_cmd)
	 ; ("search", search_cmd)
	 ; ("merge", merge_cmd)
	 ]))

let () = main ()

