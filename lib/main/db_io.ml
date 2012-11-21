open Core.Std

module Shell = Core_extended.Std.Shell

let sh ~input cmd = Shell.run_full "/bin/sh" ["-c"; cmd]

let read ~cmd fname =
  if Sys.file_exists fname = `Yes then
    let input = In_channel.read_all fname in
    match Result.try_with (fun () -> sh ~input cmd) with
      | Result.Ok s ->
	Result.try_with (fun () -> Db.of_string s)
      | Result.Error err ->
	Result.Error err
  else
    Result.Ok (Db.make ())

let write db ~cmd fname =
  let input = Db.to_string db in
  let data  = sh ~input cmd in
  Result.try_with (fun () -> Out_channel.write_all ~data fname)
