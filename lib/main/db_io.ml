open Core.Std

let sh ~input cmd = Opass_shell.sh ~input (cmd ^ " 2>/dev/null")

let read_wrap ~cmd fname =
  let open Result.Monad_infix in
  if Sys.file_exists fname = `Yes then
    let input = In_channel.read_all fname in
    Result.try_with (fun () -> sh ~input cmd) >>= fun s ->
    Result.try_with (fun () -> Db.of_string s)
  else
    Result.Ok (Db.make ())

  let read ~cmd fname =
    match read_wrap ~cmd fname with
      | Result.Ok db   -> Result.Ok db
      | Result.Error _ -> Result.Error `Bad_database

let write db ~cmd fname =
  let input = Db.to_string db in
  let data  = sh ~input cmd in
  Result.try_with (fun () -> Out_channel.write_all ~data fname)
