open Core.Std
open Result.Monad_infix

module Shell = Core_extended.Std.Shell

module Flag = struct

  open Command.Spec

  let db_file () =
    flag "-db" ~doc:" Database file, set to opass.db by default"
      (optional_with_default "opass.db" string)

  let in_all () =
    flag ~aliases:["--in-all"] "-in-all" ~doc:" Search everywhere, even in the text of notes"
      (no_arg)

  let pass_length () =
    flag "-length" ~doc:" Length of password to generate"
      (optional_with_default 24 int)

  let charset () =
    flag "-charset" ~doc:" Charset to use to generate (any, alpha, alphanum)"
      (optional_with_default "any" string)

  let src_type () =
    flag "-type" ~doc:" Source type (1password)"
      (required string)

  let src_file () =
    flag "-file" ~doc:" Source file"
      (required string)
end

let read_cmd  = "gpg --decrypt"
let write_cmd = "gpg -a --symmetric"


let safe_write db db_file =
  let db_file_bak = db_file ^ ".bak" in

  if Sys.file_exists db_file = `Yes then
    Shell.cp db_file db_file_bak;

  match Db_io.write db ~cmd:write_cmd db_file with
    | Ok () ->
      Ok ()
    | Error _ -> begin
      if Sys.file_exists db_file_bak = `Yes then
        Shell.cp db_file_bak db_file;
      Error `Database_write_fail
    end

let print_row = function
  | (name, Db.Row.Password { Db.Row.location; username; password}) ->
    Printf.printf "Name: %s\nLocation: %s\nUsername: %s\nPassword: %s\n" name
      location username password
  | (name, Db.Row.Note n) ->
    Printf.printf "Name: %s\nNote:\n%s\n" name n

let run_add ~db_file =
  let rec read_db () =
    Db_io.read ~cmd:read_cmd db_file
    >>= fun db ->
    read_row db
  and read_row db =
    Oui.read_row ()
    >>= fun row ->
    print_row row;
    confirm_row db row
  and confirm_row db row =
    Printf.printf "Add, retry, cancel? (A/r/c): %!";
    match In_channel.input_line stdin with
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
      printf "Entry added\n"
    | Error `Bad_editor ->
      printf "No proper editor found, aborting\n"
    | Error `Cancelled ->
      ()
    | Error `Duplicate ->
      printf "Row already exists, aborting\n"
    | Error `Database_write_fail ->
      printf "Writing database failed, aborting\n"
    | Error `Bad_database ->
      printf "Database is bad, aborting\n"

let run_search ~db_file ~term ~in_all =
  let rec read_db () =
    Db_io.read ~cmd:read_cmd db_file
    >>= fun db ->
    search_db db
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
            | (n, R.Note note_text) ->
              (is_sub ~substring:term n || (in_all && is_sub ~substring:term note_text))
        end
    in
    let rows = Db.search ~f db
    in
    print_rows rows
  and print_rows = function
    | [] ->
      Error `Not_found
    | rows -> begin
      List.iter
        ~f:(fun r ->
          printf "=============================================\n";
          print_row r;
          printf "---------------------------------------------\n\n")
        rows;
      Ok ()
    end
  in
  match read_db () with
    | Ok () -> ()
    | Error `Not_found ->
      printf "No rows found\n"
    | Error `Bad_database ->
      printf "Bad database, aborting\n"

let run_edit ~db_file ~entry =
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
    print_row row;
    confirm_edit db row
  and confirm_edit db rows =
    printf "Save row? (Y/n): %!";
    match In_channel.input_line stdin with
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
    printf "No rows found\n"
  | Error `Bad_database ->
    printf "Bad database, aborting\n"
  | Error `Database_write_fail ->
    printf "Writng database failed, aborting\n"
  | Error `Cancelled ->
    printf "Edit cancelled\n"
  | Error `Duplicate ->
    printf "Entry already exists, aborting\n"
  | Error `Bad_editor ->
    printf "No appropriate editor could be run\n"


let run_password ~pass_length:l ~charset:c =
  let pass =
    match c with
      | "any"      -> Password.mk_all l
      | "alpha"    -> Password.mk_alpha l
      | "alphanum" -> Password.mk_alphanum l
      | _          -> failwith "Unknown charset"
  in
  Out_channel.output_string stdout (pass ^ "\n")

let run_merge ~db_file ~src_type ~src_file =
  let rec src_to_db () =
    match src_type with
      | "1password" ->
        import_1password ()
      | _ ->
        Error `Unknown_src_type
  and import_1password () =
    match Import_1password.import src_file with
      | Some db_1password ->
        read_db db_1password
      | None ->
        Error `Bad_src_file
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
      printf "Successfully merged\n"
    | Error `Unknown_src_type ->
      printf "Source type '%s' is unknown, aborting\n" src_type
    | Error `Bad_database ->
      printf "Bad database, aborting\n"
    | Error `Duplicate ->
      printf "There were duplicates in merging, aborting\n"
    | Error `Bad_src_file ->
      printf "Bad source file, exiting\n"
    | Error `Database_write_fail ->
      printf "Failed to write database\n"

let run_del ~db_file ~terms =
  let is_member elt l =
    List.findi ~f:(fun _ e -> elt = e) l <> None
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
      List.iter
        ~f:(fun (n, _) -> printf "%s\n" n)
        rows;
      confirm_delete db rows
    end
  and confirm_delete db rows =
    printf "Delete rows? (Y/n): %!";
    match In_channel.input_line stdin with
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
      printf "No rows found\n"
    | Error `Bad_database ->
      printf "Bad database, aborting\n"
    | Error `Database_write_fail ->
      printf "Writng database failed, aborting\n"
    | Error `Cancelled ->
      printf "Delete cancelled\n"

let add_cmd = Command.basic
  ~summary:"Add an entry"
  Command.Spec.(empty +> Flag.db_file ())
  (fun db_file () -> run_add ~db_file)

let search_cmd = Command.basic
  ~summary:"Perform a search"
  Command.Spec.(empty
                +> Flag.db_file ()
                +> Flag.in_all ()
                +> anon (maybe ("term" %: string)))
  (fun db_file in_all term () -> run_search ~db_file ~term ~in_all)

let edit_cmd = Command.basic
  ~summary:"Edit an entry"
  Command.Spec.(empty
                +> Flag.db_file ()
                +> anon ("entry" %: string))
  (fun db_file entry () -> run_edit ~db_file ~entry)

let password_cmd = Command.basic
  ~summary:"Generate a random password"
  Command.Spec.(empty
                +> Flag.pass_length ()
                +> Flag.charset ())
  (fun pass_length charset () -> run_password ~pass_length ~charset)

let merge_cmd = Command.basic
  ~summary:"Merge another database into this one"
  Command.Spec.(empty
                +> Flag.db_file ()
                +> Flag.src_type ()
                +> Flag.src_file ())
  (fun db_file src_type src_file () ->
    run_merge ~db_file ~src_type ~src_file)

let del_cmd = Command.basic
  ~summary:"Delete entries"
  Command.Spec.(empty
                +> Flag.db_file ()
                +> anon ("term" %: string)
                +> anon (sequence ("term" %: string)))
  (fun db_file term_hd term_tl () ->
    run_del ~db_file ~terms:(term_hd::term_tl))

let main () =
  Random.self_init ();
  Exn.handle_uncaught ~exit:true (fun () ->
    Command.run ~version:"0.1" ~build_info:"N/A"
      (Command.group ~summary:"opass commands"
         [ "add"       , add_cmd
         ; "search"    , search_cmd
         ; "edit"      , edit_cmd
         ; "password"  , password_cmd
         ; "merge"     , merge_cmd
         ; "del"       , del_cmd
         ]))

let () = main ()

