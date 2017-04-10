let key_find k kv = CCList.Assoc.get ~eq:(=) k kv

let add_password kv db =
  let open CCOpt.Infix in
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
    | Ok db   -> Some db
    | Error _ -> None

let add_note kv db =
  let open CCOpt.Infix in
  key_find "title" kv >>= fun name ->
  key_find "notes" kv >>= fun note ->
  let module R = Db.Row in
  let r = Str.regexp "\\\\n" in
  let row = (name, R.Note (Str.global_replace r "\n" note)) in
  match Db.add row db with
    | Ok db   -> Some db
    | Error _ -> None

let rec read_next_line db headers = function
  | [] -> Some db
  | l::ls -> begin
    let open CCOpt.Infix in
    let kv = List.combine headers (CCString.Split.list_cpy ~by:"\t" l) in
    match key_find "notes" kv with
      | Some "" ->
        add_password kv db
        >>= fun db ->
        read_next_line db headers ls
      | Some _ ->
        add_note kv db
        >>= fun db ->
        read_next_line db headers ls
      | None ->
        None
  end

let read_first_line = function
  | [] ->
    None
  | l::ls ->
    let headers = CCString.Split.list_cpy ~by:"\t" l in
    read_next_line (Db.make ()) headers ls

let import fname =
  CCIO.with_in
    fname
    (fun ic ->
       let lines = CCIO.read_lines_l ic in
       read_first_line lines)
