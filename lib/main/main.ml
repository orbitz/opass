open Core.Std

module Shell = Core_extended.Std.Shell

module Command = Core_extended.Deprecated_command

let db_file = ref "opass.db"
let db_file_f =
  Command.Flag.set_string "-db" db_file
    ~doc:" Database file, set to opass.db by default"

let pass_length = ref 24
let pass_length_f =
  Command.Flag.set_int "-l" pass_length
    ~doc:" Length of password to generate"

let charset = ref "any"
let charset_f =
  Command.Flag.set_string "-c" charset
    ~doc:" Charset to use to generate (any, alpha, alphanum)"

let src_type = ref ""
let src_type_f =
  Command.Flag.set_string "-t" src_type
    ~doc:" Source type (1password)"

let src_file = ref ""
let src_file_f =
  Command.Flag.set_string "-f" src_file
    ~doc:" Source file"

let read_cmd = ref "gpg --decrypt"
let write_cmd = ref "gpg -a --symmetric"

let safe_write db db_file =
  let db_file_bak = db_file ^ ".bak" in

  if Sys.file_exists db_file = `Yes then
    Shell.cp db_file db_file_bak;

  match Db_io.write db ~cmd:!write_cmd db_file with
    | Result.Ok () ->
      Result.Ok ()
    | Result.Error _ -> begin
      if Sys.file_exists db_file_bak = `Yes then
	Shell.cp db_file_bak db_file;
      Result.Error `Database_write_fail
    end

let print_row = function
  | (name, Db.Row.Password { Db.Row.location = l
			   ;        username = u
			   ;        password = p
			   }) ->
    Printf.printf "Name: %s\nLocation: %s\nUsername: %s\nPassword: %s\n" name l u p
  | (name, Db.Row.Note n) ->
    Printf.printf "Name: %s\nNote:\n%s\n" name n

let run_add db_file =
  let rec read_db () =
    match Db_io.read ~cmd:!read_cmd db_file with
      | Result.Ok db ->
	read_row db
      | Result.Error `Bad_database ->
	Result.Error `Bad_database
  and read_row db =
    match Oui.read_row () with
      | Result.Ok row -> begin
	print_row row;
	confirm_row db row
      end
      | Result.Error `Bad_editor ->
	Result.Error `Bad_editor
      | Result.Error `Cancelled ->
	Result.Error `Cancelled
  and confirm_row db row =
    Printf.printf "Add, retry, cancel? (A/r/c): %!";
    match In_channel.input_line stdin with
      | Some l -> confirm_action db row l
      | None ->
	Result.Error `Cancelled
  and confirm_action db row = function
    | "" | "A" | "a" -> add_row db row
    | "R" | "r"      -> read_row db
    | "C" | "c"      -> Result.Error `Cancelled
    | _              -> confirm_row db row
  and add_row db row =
    match Db.add row db with
      | Result.Ok db ->
	safe_write db db_file
      | Result.Error `Duplicate ->
	Result.Error `Duplicate
  in
  match read_db () with
    | Result.Ok () ->
      Printf.printf "Entry added\n"
    | Result.Error `Bad_editor ->
      Printf.printf "No proper editor found, aborting\n"
    | Result.Error `Cancelled ->
      ()
    | Result.Error `Duplicate ->
      Printf.printf "Row already exists, aborting\n"
    | Result.Error `Database_write_fail ->
      Printf.printf "Writing database failed, aborting\n"
    | Result.Error `Bad_database ->
      Printf.printf "Database is bad, aborting\n"

let run_search db_file term =
  let rec read_db () =
    match Db_io.read ~cmd:!read_cmd db_file with
      | Result.Ok db ->
	search_db db
      | Result.Error `Bad_database ->
	Result.Error `Bad_database
  and search_db db =
    let is_sub ~substring s =
      Core_extended.Std.String.is_substring
	~substring:(String.lowercase substring)
	(String.lowercase s)
    in
    let module R = Db.Row in
    let f =
      match term with
	| None -> Fn.const true
	| Some term -> begin
	  function
	    | (n, R.Password { R.location = l
			     ;   username = u
			     }) ->
	      (is_sub ~substring:term n ||
		 is_sub ~substring:term l  ||
		 is_sub ~substring:term u)
	    | (n, R.Note _) ->
	      is_sub ~substring:term n
	end
    in
    let rows = Db.search ~f db
    in
    print_rows rows
  and print_rows = function
    | [] ->
      Result.Error `Not_found
    | rows -> begin
      List.iter
	~f:(fun r ->
	  Printf.printf "=============================================\n";
	  print_row r)
	rows;
      Result.Ok ()
    end
  in
  match read_db () with
    | Result.Ok () -> ()
    | Result.Error `Not_found ->
      Printf.printf "No rows found\n"
    | Result.Error `Bad_database ->
      Printf.printf "Bad database, aborting\n"

let run_edit db_file entryname =
  let rec read_db () =
    match Db_io.read ~cmd:!read_cmd db_file with
      | Result.Ok db ->
	search_db db
      | Result.Error `Bad_database ->
	Result.Error `Bad_database
  and search_db db =
    let module R = Db.Row in
    let f = fun (n, _) -> n = entryname in
    match Db.search ~f db with
      | [] ->
	Result.Error `Not_found
      | [row] ->
	edit_row db row
      | _ ->
	failwith "This should never happen"
  and edit_row db row =
    match Oui.edit_row row with
      | Result.Ok row -> begin
	print_row row;
	confirm_edit db row
      end
      | Result.Error `Bad_editor ->
	Result.Error `Bad_editor
      | Result.Error `Cancelled ->
	Result.Error `Cancelled
  and confirm_edit db rows =
    Printf.printf "Save row? (Y/n): %!";
    match In_channel.input_line stdin with
      | Some l -> check_input db rows l
      | None   -> Result.Error `Cancelled
  and check_input db row = function
    | "" | "Y" | "y" -> save_row db row
    | "N" | "n"      -> Result.Error `Cancelled
    | _              -> confirm_edit db row
  and save_row db row =
    match Db.add row (Db.delete entryname db) with
      | Result.Ok db ->
	safe_write db db_file
      | Result.Error `Duplicate ->
	Result.Error `Duplicate
  in
  match read_db () with
    | Result.Ok () -> ()
    | Result.Error `Not_found ->
      Printf.printf "No rows found\n"
    | Result.Error `Bad_database ->
      Printf.printf "Bad database, aborting\n"
    | Result.Error `Database_write_fail ->
      Printf.printf "Writng database failed, aborting\n"
    | Result.Error `Cancelled ->
      Printf.printf "Edit cancelled\n"
    | Result.Error `Duplicate ->
      Printf.printf "Entry already exists, aborting\n"
    | Result.Error `Bad_editor ->
      Printf.printf "No appropriate editor could be run\n"


let run_password l c =
  let pass =
    match c with
      | "any"      -> Password.mk_all l
      | "alpha"    -> Password.mk_alpha l
      | "alphanum" -> Password.mk_alphanum l
      | _          -> failwith "Unknown charset"
  in
  Out_channel.output_string stdout (pass ^ "\n")

let run_merge db_file src_type src_file =
  let rec src_to_db () =
    match src_type with
      | "1password" ->
	import_1password ()
      | _ ->
	Result.Error `Unknown_src_type
  and import_1password () =
    match Import_1password.import src_file with
      | Some db_1password ->
	read_db db_1password
      | None ->
	Result.Error `Bad_src_file
  and read_db src_db =
    match Db_io.read ~cmd:!read_cmd db_file with
      | Result.Ok db ->
	merge_db src_db db
      | Result.Error `Bad_database ->
	Result.Error `Bad_database
  and merge_db src_db db =
    match Db.merge src_db db with
      | Result.Ok db ->
	safe_write db db_file
      | Result.Error `Duplicate ->
	Result.Error `Duplicate
  in
  match src_to_db () with
    | Result.Ok () ->
      Printf.printf "Successfully merged\n"
    | Result.Error `Unknown_src_type ->
      Printf.printf "Source type '%s' is unknown, aborting\n" src_type
    | Result.Error `Bad_database ->
      Printf.printf "Bad database, aborting\n"
    | Result.Error `Duplicate ->
      Printf.printf "There were duplicates in merging, aborting\n"
    | Result.Error `Bad_src_file ->
      Printf.printf "Bad source file, exiting\n"
    | Result.Error `Database_write_fail ->
      Printf.printf "Failed to write database\n"

let run_del db_file terms =
  let is_member elt l =
    List.findi ~f:(fun _ e -> elt = e) l <> None
  in
  let rec read_db () =
    match Db_io.read ~cmd:!read_cmd db_file with
      | Result.Ok db ->
	search_db db
      | Result.Error `Bad_database ->
	Result.Error `Bad_database
  and search_db db =
    let module R = Db.Row in
    let f = fun (n, _) -> is_member n terms in
    let rows = Db.search ~f db in
    print_rows db rows
  and print_rows db = function
    | [] ->
      Result.Error `Not_found
    | rows -> begin
      List.iter
	~f:(fun (n, _) -> Printf.printf "%s\n" n)
	rows;
      confirm_delete db rows
    end
  and confirm_delete db rows =
    Printf.printf "Delete rows? (Y/n): %!";
    match In_channel.input_line stdin with
      | Some l -> check_input db rows l
      | None   -> Result.Error `Cancelled
  and check_input db rows = function
    | "" | "Y" | "y" -> delete_rows db rows
    | "N" | "n"      -> Result.Error `Cancelled
    | _              -> confirm_delete db rows
  and delete_rows db = function
    | [] -> safe_write db db_file
    | (n, _)::ns -> delete_rows (Db.delete n db) ns
  in
  match read_db () with
    | Result.Ok () -> ()
    | Result.Error `Not_found ->
      Printf.printf "No rows found\n"
    | Result.Error `Bad_database ->
      Printf.printf "Bad database, aborting\n"
    | Result.Error `Database_write_fail ->
      Printf.printf "Writng database failed, aborting\n"
    | Result.Error `Cancelled ->
      Printf.printf "Delete cancelled\n"

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
    | []       -> None
    | [search] -> Some search
    | _        -> failwith "Need search term")
  (fun search -> run_search !db_file search)

let edit_cmd = Command.create_no_accum
  ~summary:"Edit an entry"
  ~usage_arg:"[-db file] entryname"
  ~flags:[db_file_f]
  ~final:(function
    | [entry] -> entry
    | _        -> failwith "Must provide one entry name")
  (fun entry -> run_edit !db_file entry)

let password_cmd = Command.create_no_accum
  ~summary:"Generate a random password"
  ~usage_arg:"[-l] [-c charset]"
  ~flags:[pass_length_f; charset_f]
  ~final:(function
    | [] -> ()
    | _  -> failwith "No arguments should be specifeid")
  (fun () -> run_password !pass_length !charset)

let merge_cmd = Command.create_no_accum
  ~summary:"Merge another database into this one"
  ~usage_arg:"[-db file] -t type -f file"
  ~flags:[db_file_f; src_type_f; src_file_f]
  ~final:(function
    | [] -> ()
    | _  -> failwith "No arguments should be specified")
  (fun () -> run_merge !db_file !src_type !src_file)

let del_cmd = Command.create_no_accum
  ~summary:"Delete entries"
  ~usage_arg:"[-db file] term1 [term2 [term3 [..]]]"
  ~flags:[db_file_f]
  ~final:(function
    | []    -> failwith "Need atleast one search term"
    | terms -> terms)
  (fun terms -> run_del !db_file terms)

let main () =
  Random.self_init ();
  Exn.handle_uncaught ~exit:true (fun () ->
    Command.run ~version:"0.1" ~build_info:"N/A"
      (Command.group ~summary:"opass commands"
	 [ ("add", add_cmd)
	 ; ("search", search_cmd)
	 ; ("edit", edit_cmd)
	 ; ("password", password_cmd)
	 ; ("merge", merge_cmd)
	 ; ("del", del_cmd)
	 ]))

let () = main ()

