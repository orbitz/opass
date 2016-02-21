open Core.Std

let (|>) x f = f x;;

let finally handler f x =
  let r = (try f x with e -> handler(); raise e) in
  handler ();
  r
;;

let with_dispose ~dispose f x =
  finally (fun () -> dispose x) f x
;;

let rec fold_on_dir_tree f acc start_dir =
  let rec loop_the_dir handle acc =
    let entry =
      try Some (Unix.readdir handle)
      with End_of_file -> None in

    match entry with
    | None -> acc   (* No more entry *)
    | Some entry -> begin (* another entry to process *)
        if entry <> "." && entry <> ".." (* skip . and .. *)
        then begin
          let file_name = Filename.concat start_dir entry in
          let kind =
            try Some ((Unix.stat file_name).Unix.st_kind)
            with Unix.Unix_error _ -> None in

          match kind with
          | Some kind -> begin
              let new_acc = f kind acc file_name in
              match kind with
              | Unix.S_DIR -> loop_the_dir handle (fold_on_dir_tree f new_acc file_name)
              | _ -> loop_the_dir handle new_acc
            end
          | None -> loop_the_dir handle acc (* cannot stat, continue *)
        end
        else loop_the_dir handle acc (* continue looping *)
      end
  in (* loop_the_dir *)

  let dispose h = Unix.closedir h in
  with_dispose ~dispose (fun h -> loop_the_dir h acc) (Unix.opendir start_dir)
;;

let note_of_file kind db fname =
  let module F = Filename in
  let module S = Core_kernel.Core_string in
  let name = F.basename fname |>
             F.chop_extension |>
             S.tr ~target:'_' ~replacement:' ' in
  let name = List.map (S.split name ~on:' ') S.capitalize |>
             S.concat ~sep:" " in
  let content = open_in_bin fname |> In_channel.input_all |> S.strip in
  let res = Db.add (name, Db.Row.Note content) db in
  match res with
  | Ok x -> x
  | Error _ -> begin
      Printf.eprintf "Error: duplicate entry: \"%s\", going on...\n%!" name;
      db
    end
;;

let import dirname =
  Some (fold_on_dir_tree note_of_file (Db.make ()) dirname)
;;
