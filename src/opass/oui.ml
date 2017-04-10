let k k kv =
  CCList.Assoc.get_exn ~eq:(=) k kv

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
  match Editable.run [pf] with
    | Ok [inputs] ->
      let module R = Db.Row in
      let password =
	generate_password
	  (k "password" inputs)
	  (int_of_string (k "length" inputs))
	  (k "charset" inputs)
      in
      Ok
	(k "name" inputs, R.Password { R.location = k "location" inputs
				     ;   username = k "username" inputs
				     ;   password = password
				     })
    | Ok _ ->
      failwith "Bad input"
    | Error `Cancelled ->
      Error `Cancelled
    | Error `Bad_editor ->
      Error `Bad_editor

let run_note nf =
  match Editable.run [nf] with
    | Ok [inputs] ->
      let module R = Db.Row in
      Ok (k "name" inputs, R.Note (k "note" inputs))
    | Ok _ ->
      failwith "Bad input"
    | Error `Cancelled ->
      Error `Cancelled
    | Error `Bad_editor ->
      Error `Bad_editor

let rec read_row () =
  Printf.printf "Password/note (P/n): %!";
  match CCIO.read_line stdin with
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
