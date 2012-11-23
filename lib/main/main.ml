open Core.Std

module Shell = Core_extended.Std.Shell

module Command = Core_extended.Deprecated_command

let db_file = ref "opass.db"
let db_file_f =
  Command.Flag.set_string "-db" db_file
    ~doc:" Database file, set to opass.db by default"


let read_cmd = ref "gpg --decrypt"
let write_cmd = ref "gpg -a --symmetric"

let add_or_exn row db =
  match Db.add row db with
    | Result.Ok db   -> db
    | Result.Error _ -> failwith "Addition failed"

let safe_write db db_file =
  let db_file_bak = db_file ^ ".bak" in

  if Sys.file_exists db_file = `Yes then
    Shell.cp db_file db_file_bak;

  match Db_io.write db ~cmd:!write_cmd db_file with
    | Result.Ok () ->
      ()
    | Result.Error _ -> begin
      if Sys.file_exists db_file_bak = `Yes then
	Shell.cp db_file_bak db_file;
      failwith "Could not write db"
    end

let run_add db_file =
  let db = Result.ok_exn (Db_io.read ~cmd:!read_cmd db_file) in
  let row = Result.ok_exn (Oui.read_row ()) in
  let db  = add_or_exn row db in
  safe_write db db_file

let run_search db_file term =
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

let main () =
  Exn.handle_uncaught ~exit:true (fun () ->
    Command.run ~version:"0.1" ~build_info:"N/A"
      (Command.group ~summary:"opass commands"
	 [ ("add", add_cmd)
	 ; ("search", search_cmd)
	 ]))

let () = main ()

