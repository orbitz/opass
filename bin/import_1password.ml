open Core.Std

let key_find k kv = List.Assoc.find kv ~equal:(=) k

let add_password kv db =
  let open Option.Monad_infix in
  key_find "title" kv        >>= fun name ->
  key_find "username" kv     >>= fun username ->
  key_find "password" kv     >>= fun password ->
  key_find "URL/Location" kv >>= fun location ->
  let module R = Db.Row in
  let row = (name, R.Password { R.location = location
			      ;   username = username
			      ;   password = password
			      })
  in
  match Db.add row db with
    | Result.Ok db   -> Some db
    | Result.Error _ -> None

let add_note kv db =
  let open Option.Monad_infix in
  key_find "title" kv >>= fun name ->
  key_find "notes" kv >>= fun note ->
  let module R = Db.Row in
  let r = Str.regexp "\\\\n" in
  let row = (name, R.Note (Str.global_replace r "\n" note)) in
  match Db.add row db with
    | Result.Ok db   -> Some db
    | Result.Error _ -> None

let rec read_next_line db headers = function
  | [] -> Some db
  | l::ls -> begin
    let open Option.Monad_infix in
    List.zip headers (String.split ~on:'\t' l) >>= fun kv ->
    match key_find "notes" kv with
      | Some "" -> add_password kv db >>= fun db -> read_next_line db headers ls
      | Some _  -> add_note kv db     >>= fun db -> read_next_line db headers ls
      | None    -> None
  end

let read_first_line = function
  | [] ->
    None
  | l::ls ->
    let headers = String.split ~on:'\t' l in
    read_next_line (Db.make ()) headers ls

let import fname =
  In_channel.with_file
    fname
    ~f:(fun fin ->
      let lines = In_channel.input_lines fin in
      read_first_line lines)
