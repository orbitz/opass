open CCResult.Infix

let cp src dst =
  if Sys.command (Printf.sprintf "cp -v %s %s" src dst) <> 0 then
    failwith "Error in copy"

let read_cmd  = "gpg --decrypt"
let write_cmd = "gpg -a --symmetric"


let safe_write db db_file =
  let db_file_bak = db_file ^ ".bak" in

  if Sys.file_exists db_file then
    cp db_file db_file_bak;

  match Db_io.write db ~cmd:write_cmd db_file with
    | Ok () ->
      Ok ()
    | Error _ -> begin
      if Sys.file_exists db_file_bak then
        cp db_file_bak db_file;
      Error `Database_write_fail
    end

let print_row show_pass = function
  | (name, Db.Row.Password { Db.Row.location; username; password}) -> begin
    Printf.printf "Name: %s\nLocation: %s\nUsername: %s\n"
      name
      location
      username;
    if show_pass then
      Printf.printf "Password: %s\n" password;
  end
  | (name, Db.Row.Note n) ->
    Printf.printf "Name: %s\nNote:\n%s\n" name n

let copy_password password prog =
  ignore (Opass_shell.sh ~input:password prog)

let maybe_copy_password copy_pass copy_pass_prog = function
  | [(_, Db.Row.Password { Db.Row.password })] -> begin
    match (copy_pass, copy_pass_prog) with
      | (false, _)        -> ()
      | (true, Some prog) -> copy_password password prog
      | (true, None)      -> assert false
  end
  | _ ->
    ()

let run_add db_file =
  let rec read_db () =
    Db_io.read ~cmd:read_cmd db_file
    >>= fun db ->
    read_row db
  and read_row db =
    Oui.read_row ()
    >>= fun row ->
    print_row true row;
    confirm_row db row
  and confirm_row db row =
    Printf.printf "Add, retry, cancel? (A/r/c): %!";
    match CCIO.read_line stdin with
    | Some l -> confirm_action db row l
    | None   -> Error `Cancelled
  and confirm_action db row = function
    | "" | "A" | "a" -> add_row db row
    | "R" | "r"      -> read_row db
    | "C" | "c"      -> Error `Cancelled
    | _              -> confirm_row db row
  and add_row db row =
    Db.add row db
    >>= fun db ->
    safe_write db db_file
  in
  match read_db () with
    | Ok () ->
      Printf.printf "Entry added\n"
    | Error `Bad_editor ->
      Printf.printf "No proper editor found, aborting\n"
    | Error `Cancelled ->
      ()
    | Error `Duplicate ->
      Printf.printf "Row already exists, aborting\n"
    | Error `Database_write_fail ->
      Printf.printf "Writing database failed, aborting\n"
    | Error `Bad_database ->
      Printf.printf "Database is bad, aborting\n"

let is_substring ~substring haystack =
  CCString.find ~sub:substring haystack <> -1

let run_search db_file terms in_all pattern show_pass copy_pass copy_pass_prog =
  let rec read_db () =
    Db_io.read ~cmd:read_cmd db_file
    >>= fun db ->
    search_db db
  and search_db db =
    let is_sub ~substring s =
      if pattern then
        match Lua_pattern.of_string substring with
          | Some p ->
            Lua_pattern.mtch s p <> None
          | None ->
            failwith (Printf.sprintf "Invalid pattern: %s" substring)
      else
        is_substring
          ~substring:(CCString.lowercase_ascii substring)
          (CCString.lowercase_ascii s)
    in
    let matches_all ~terms s =
      ListLabels.fold_left
        ~f:(fun acc substring -> is_sub ~substring s && acc)
        ~init:true
        terms
    in
    let module R = Db.Row in
    let f =
      match terms with
        | [] -> CCFun.const true
        | terms ->
          begin function
            | (n, R.Password { R.location = l
                             ;   username = u
                             }) ->
              (matches_all ~terms n ||
                 matches_all ~terms l  ||
                 matches_all ~terms u)
            | (n, R.Note note_text) ->
              (matches_all ~terms n || (in_all && matches_all ~terms note_text))
        end
    in
    let rows = Db.search ~f db
    in
    let ret = print_rows show_pass rows in
    flush stdout;
    maybe_copy_password copy_pass copy_pass_prog rows;
    ret
  and print_rows show_pass = function
    | [] ->
      Error `Not_found
    | rows -> begin
      ListLabels.iter
        ~f:(fun r ->
            Printf.printf "=============================================\n";
            print_row show_pass r;
            Printf.printf "---------------------------------------------\n\n")
        rows;
      Ok ()
    end
  in
  match read_db () with
    | Ok () -> ()
    | Error `Not_found ->
      Printf.printf "No rows found\n"
    | Error `Bad_database ->
      Printf.printf "Bad database, aborting\n"

let run_edit db_file entry =
  let rec read_db () =
    Db_io.read ~cmd:read_cmd db_file
    >>= fun db ->
    search_db db
  and search_db db =
    let module R = Db.Row in
    match Db.search ~f:(fun (n,_) -> n = entry) db with
    | [] ->
      Error `Not_found
    | [row] ->
      edit_row db row
    | _ ->
      failwith "This should never happen"
  and edit_row db row =
    Oui.edit_row row
    >>= fun row ->
    print_row true row;
    confirm_edit db row
  and confirm_edit db rows =
    Printf.printf "Save row? (Y/n): %!";
    match CCIO.read_line stdin with
    | Some l -> check_input db rows l
    | None   -> Error `Cancelled
  and check_input db row = function
    | "" | "Y" | "y" -> save_row db row
    | "N" | "n"      -> Error `Cancelled
    | _              -> confirm_edit db row
  and save_row db row =
    Db.add row (Db.delete entry db)
    >>= fun db ->
    safe_write db db_file
  in
  match read_db () with
  | Ok () -> ()
  | Error `Not_found ->
    Printf.printf "No rows found\n"
  | Error `Bad_database ->
    Printf.printf "Bad database, aborting\n"
  | Error `Database_write_fail ->
    Printf.printf "Writng database failed, aborting\n"
  | Error `Cancelled ->
    Printf.printf "Edit cancelled\n"
  | Error `Duplicate ->
    Printf.printf "Entry already exists, aborting\n"
  | Error `Bad_editor ->
    Printf.printf "No appropriate editor could be run\n"


let run_password l c =
  let pass =
    match c with
      | "any"      -> Password.mk_all l
      | "alpha"    -> Password.mk_alpha l
      | "alphanum" -> Password.mk_alphanum l
      | _          -> failwith "Unknown charset"
  in
  output_string stdout (pass ^ "\n")

let run_merge db_file src_type src_file =
  let rec src_to_db () =
    match src_type with
      | "1password" -> import_1password ()
      | "txt" -> import_txt_dir ()
      | "csv" -> import_csv ()
      | _ ->
        Error `Unknown_src_type
  and import_1password () =
    match Import_1password.import src_file with
      | Some db_1password ->
        read_db db_1password
      | None ->
        Error `Bad_src_file
  and import_txt_dir () =
    match Import_txt.import src_file with
      | Some db_txt -> read_db db_txt
      | None -> Error `Bad_src_file
  and import_csv () =
    match Import_csv.import src_file with
      | Some db_txt -> read_db db_txt
      | None -> Error `Bad_src_file
  and read_db src_db =
    Db_io.read ~cmd:read_cmd db_file
    >>= fun db ->
    merge_db src_db db
  and merge_db src_db db =
    Db.merge src_db db
    >>= fun db ->
    safe_write db db_file
  in
  match src_to_db () with
    | Ok () ->
      Printf.printf "Successfully merged\n"
    | Error `Unknown_src_type ->
      Printf.printf "Source type '%s' is unknown, aborting\n" src_type
    | Error `Bad_database ->
      Printf.printf "Bad database, aborting\n"
    | Error `Duplicate ->
      Printf.printf "There were duplicates in merging, aborting\n"
    | Error `Bad_src_file ->
      Printf.printf "Bad source file, exiting\n"
    | Error `Database_write_fail ->
      Printf.printf "Failed to write database\n"

let run_del db_file terms =
  let is_member elt l =
    CCList.find_pred (fun e -> elt = e) l <> None
  in
  let rec read_db () =
    Db_io.read ~cmd:read_cmd db_file
    >>= fun db ->
    search_db db
  and search_db db =
    let module R = Db.Row in
    let f = fun (n, _) -> is_member n terms in
    let rows = Db.search ~f db in
    print_rows db rows
  and print_rows db = function
    | [] ->
      Error `Not_found
    | rows -> begin
      ListLabels.iter
        ~f:(fun (n, _) -> Printf.printf "%s\n" n)
        rows;
      confirm_delete db rows
    end
  and confirm_delete db rows =
    Printf.printf "Delete rows? (Y/n): %!";
    match CCIO.read_line stdin with
    | Some l -> check_input db rows l
    | None   -> Error `Cancelled
  and check_input db rows = function
    | "" | "Y" | "y" -> delete_rows db rows
    | "N" | "n"      -> Error `Cancelled
    | _              -> confirm_delete db rows
  and delete_rows db = function
    | [] -> safe_write db db_file
    | (n, _)::ns -> delete_rows (Db.delete n db) ns
  in
  match read_db () with
    | Ok () -> ()
    | Error `Not_found ->
      Printf.printf "No rows found\n"
    | Error `Bad_database ->
      Printf.printf "Bad database, aborting\n"
    | Error `Database_write_fail ->
      Printf.printf "Writng database failed, aborting\n"
    | Error `Cancelled ->
      Printf.printf "Delete cancelled\n"

module Cmdline = struct
  module C = Cmdliner

  let db_file =
    let doc = "Location of the database." in
    C.Arg.(value & opt string "opass.db" & info ["db"] ~docv:"FILE" ~doc)

  let in_all =
    let doc = "Search everywhere, even in the text of notes." in
    C.Arg.(value & flag & info ["in-all"] ~doc)

  let pass_length =
    let doc = "Length of the password to generate." in
    C.Arg.(value & opt int 24 & info ["length"] ~docv:"INT" ~doc)

  let charset =
    let doc = "Charset to use to generate (any, alpha, alphanum)." in
    C.Arg.(value & opt string "any" & info ["charset"] ~docv:"CHARSET" ~doc)

  let src_type =
    let doc = "Source type (1password, txt, csv)." in
    C.Arg.(required & opt (some string) None & info ["type"] ~docv:"SOURCE" ~doc)

  let src_file =
    let doc = "Source file (for 1password and csv) or directory (for txt)." in
    C.Arg.(required & opt (some file) None & info ["file"] ~docv:"FILE" ~doc)

  let show_pass =
    let doc = "Display the password instead of leaving it blank." in
    C.Arg.(value & flag & info ["s"; "show"] ~doc)

  let copy_pass =
    let doc = "Pipe the password into a program." in
    C.Arg.(value & flag & info ["c"; "copy"] ~doc)

  let copy_pass_prog =
    let doc = "Use this program to pipe." in
    let env = C.Arg.env_var "OPASS_COPY" ~doc in
    C.Arg.(value & opt (some string) None & info ["copy-prog"] ~env ~docv:"STRING" ~doc)

  let pattern =
    let doc = "The search terms will be interpreted as a Lua pattern." in
    C.Arg.(value & flag & info ["p"; "pattern"] ~doc)

  let terms =
    C.Arg.(value & pos_all string [] & info [] ~docv:"TERM")

  let entry =
    C.Arg.(required & pos 0 (some string) None & info [] ~docv:"ENTRY")

  let add_cmd =
    let doc = "Add a password or note." in
    C.Term.((const run_add $ db_file),
            info "add" ~doc)

  let search_cmd =
    let doc = "Search for a password or note." in
    C.Term.((const run_search $
             db_file $
             terms $
             in_all $
             pattern $
             show_pass $
             copy_pass $
             copy_pass_prog),
            info "search" ~doc)

  let edit_cmd =
    let doc = "Edit a password or note." in
    C.Term.((const run_edit $ db_file $ entry),
            info "edit" ~doc)

  let password_cmd =
    let doc = "Generate a random password." in
    C.Term.((const run_password $ pass_length $ charset),
            info "password" ~doc)

  let merge_cmd =
    let doc = "Merge a password source into this database." in
    C.Term.((const run_merge $ db_file $ src_type $ src_file),
            info "merge" ~doc)

  let del_cmd =
    let doc = "Delete a password or note." in
    C.Term.((const run_del $ db_file $ terms),
            info "del" ~doc)

  let default_cmd =
    let doc = "Manage encrypted passwords and notes." in
    C.Term.(ret (const (`Help (`Pager, None))),
            info "help" ~doc)

  let cmds = [add_cmd; search_cmd; edit_cmd; password_cmd; merge_cmd; del_cmd]
end

let main () =
  Random.self_init ();
  match Cmdliner.Term.eval_choice Cmdline.default_cmd Cmdline.cmds with
    | `Error _ -> exit 1
    | _ -> exit 0

let () = main ()
