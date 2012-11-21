open Core.Std

let p = Printf.printf

let read_row_option () =
  let open Option.Monad_infix in
  p "Note name: %!";
  In_channel.input_line stdin >>= fun name ->
  p "Note data: %!";
  In_channel.input_line stdin >>= fun data ->
  Option.return (name, data)

let read_row () =
  match read_row_option () with
    | Some (name, data) ->
      Result.Ok (name, Db.Row.Note data)
    | None ->
      failwith "Could not read row"

