let sh ~input cmd = Opass_shell.sh ~input (cmd ^ " 2>/dev/null")

let read_wrap ~cmd fname =
  let open CCResult.Infix in
  if Sys.file_exists fname then
    let input = CCIO.with_in fname CCIO.read_all in
    CCResult.guard (fun () -> sh ~input cmd)
    >>= fun s ->
    CCResult.guard (fun () -> Db.of_string s)
  else
    Ok (Db.make ())

  let read ~cmd fname =
    match read_wrap ~cmd fname with
      | Ok db   -> Ok db
      | Error _ -> Error `Bad_database

let write db ~cmd fname =
  let input = Db.to_string db in
  let data  = sh ~input cmd in
  CCResult.guard (fun () -> CCIO.with_out fname (CCFun.flip output_string data))
