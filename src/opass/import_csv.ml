let import filename =
  let data = Csv.load filename in
  let headers = List.hd data in
  if List.length headers = 4 then begin
    let name_col, location_col, username_col, password_col =
      ref None, ref None, ref None, ref None in
    List.iteri
      (fun index header ->
         if header = "Name" then name_col := Some index;
         if header = "Location" then location_col := Some index;
         if header = "Username" then username_col := Some index;
         if header = "Password" then password_col := Some index)
      headers;
    let name_col, location_col, username_col, password_col, data =
      match !name_col, !location_col, !username_col, !password_col with
      | Some n, Some l, Some u, Some p -> n, l, u, p, List.tl data
      | _, _, _, _ -> 0, 1, 2, 3, data in
    Some (
      List.fold_left
        (fun db record ->
           let name = List.nth record name_col in
           let pwd = {
             Db.Row.location = List.nth record location_col;
             Db.Row.username = List.nth record username_col;
             Db.Row.password = List.nth record password_col;
           } in
           let res = Db.add (name, Db.Row.Password pwd) db in
           match res with
           | Ok x -> x
           | Error _ -> begin
               Printf.eprintf "Error: duplicate entry: \"%s\", going on...\n%!" name;
               db
             end)
        (Db.make ())
        data)
  end else begin
    Printf.eprintf "Error: CSV file %s has too much columns\n%!" filename;
    None
  end
;;
