open Core.Std

let k k kv =
  List.Assoc.find_exn kv ~equal:(=) k

let generate_password p l c =
  if p <> "" then
    p
  else
    match c with
      | "any"      -> Password.mk_all l
      | "alpha"    -> Password.mk_alpha l
      | "alphanum" -> Password.mk_alphanum l
      | _          -> failwith "This should never happen"

let run_password pf =
  match Editable.Engine.run [pf] with
    | Result.Ok [inputs] ->
      let module R = Db.Row in
      let password =
	generate_password
	  (k "password" inputs)
	  (Int.of_string (k "length" inputs))
	  (k "charset" inputs)
      in
      Result.Ok
	(k "name" inputs, R.Password { R.location = k "location" inputs
				     ;   username = k "username" inputs
				     ;   password = password
				     })
    | Result.Ok _ ->
      failwith "Bad input"
    | Result.Error `Cancelled ->
      Result.Error `Cancelled
    | Result.Error `Bad_editor ->
      Result.Error `Bad_editor

let run_note nf =
  match Editable.Engine.run [nf] with
    | Result.Ok [inputs] ->
      let module R = Db.Row in
      Result.Ok (k "name" inputs, R.Note (k "note" inputs))
    | Result.Ok _ ->
      failwith "Bad input"
    | Result.Error `Cancelled ->
      Result.Error `Cancelled
    | Result.Error `Bad_editor ->
      Result.Error `Bad_editor

let rec read_row () =
  Printf.printf "Password/note (P/n): %!";
  match In_channel.input_line stdin with
    | Some l -> begin
      match l with
	| "" | "P" | "p" ->
	  run_password (Forms.password ())
	| "N" | "n" ->
	  run_note (Forms.note ())
	| _ -> begin
	  Printf.printf "Unknown input, try again\n";
	  read_row ()
	end
    end
    | None ->
      Result.Error `Cancelled


let edit_row = function
    | (name, Db.Row.Password p) ->
      run_password (Forms.password ~name ~p ())
    | (name, Db.Row.Note n) ->
      run_note (Forms.note ~name ~n ())
