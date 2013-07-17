open Core.Std

type errors = [ `Cancelled | `Bad_editor ]

let form_sep = "======================================="

let get_editors () =
  let editors = ["emacs"; "vim"; "vi"; "nano"] in
  match Sys.getenv "EDITOR" with
    | Some editor -> editor::editors
    | None        -> editors

let parse_line l =
  match String.lsplit2 ~on:':' l with
    | Some (h, s) ->
      `New (String.strip h, String.strip s)
    | None ->
      `Continue (String.strip l)

let write_forms fname forms =
  let string_of_form form =
    String.concat
      ~sep:"\n"
      (List.map
	 ~f:(fun e ->
	   Entry.prompt e ^ ": " ^ Entry.default e)
	 (Form.to_list form))
  in
  let forms_str =
    String.concat
      ~sep:(form_sep ^ "\n")
      (List.map ~f:string_of_form forms)
  in
  Out_channel.with_file
    fname
    ~f:(Fn.flip Out_channel.output_string forms_str)

let rec read_next_line form (ch, cl) acc = function
  | [] ->
    Result.Ok ((ch, cl)::acc)
  | l::ls when String.strip l = "" ->
    read_next_line form (ch, cl) acc ls
  | l::ls -> begin
    match parse_line l with
      | `New (p, s) -> begin
	match Form.name_of_prompt p form with
	  | Some h ->
	    read_next_line form (h, s) ((ch, cl)::acc) ls
	  | None ->
	    read_next_line form (ch, cl ^ "\n" ^ l) acc ls
      end
      | `Continue s ->
	read_next_line form (ch, cl ^ "\n" ^ s) acc ls
  end

let rec read_first_line form = function
  | [] ->
    Result.Error `Bad_file
  | l::ls when String.strip l = "" ->
    read_first_line form ls
  | l::ls -> begin
    match parse_line l with
      | `New (p, s) -> begin
	match Form.name_of_prompt p form with
	  | Some h ->
	    read_next_line form (h, s) [] ls
	  | None ->
	    Result.Error (`Bad_prompt p)
      end
      | `Continue _ ->
	Result.Error `Bad_file
  end

let read_forms fname forms =
  let lines =
    In_channel.with_file
      fname
      ~f:In_channel.input_lines
  in
  let remove_sep =
    List.filter ~f:(function | [v] when v = form_sep -> false | _ -> true)
  in
  let grouped =
    remove_sep
      (List.group
	 ~break:(fun x y -> x = form_sep || y = form_sep)
	 lines)
  in
  let rec read_form acc = function
    | [] -> Result.Ok (List.rev acc)
    | (form, lines)::fl -> begin
      match read_first_line form lines with
	| Result.Ok r      -> read_form (r::acc) fl
	| Result.Error err -> Result.Error err
    end
  in
  read_form [] (List.zip_exn forms grouped)

let validate_forms inputs forms =
  let rec validate_form acc = function
    | [] ->
      Result.Ok (List.rev acc)
    | (input, form)::inf -> begin
      match Form.validate input form with
	| [] ->
	  validate_form (input::acc) inf
	| errors ->
	  Result.Error errors
    end
  in
  validate_form [] (List.zip_exn inputs forms)

let rec run_editor fname = function
  | [] ->
    Result.Error `Bad_editor
  | e::es -> begin
    match Unix.system (e ^ " " ^ fname) with
      | Result.Ok () ->
	Result.Ok ()
      | Result.Error _ ->
	run_editor fname es
  end

let print_errors _ _ = Printf.printf "Errors\n"
(* let print_errors form errors = *)
(*   List.iter *)
(*     ~f:(fun (n, _) -> *)
(*       let prompt = *)
(* 	match Form.prompt_of_name n form with *)
(* 	  | Some p -> p *)
(* 	  | None   -> "Unknown" *)
(*       in *)
(*       Out_channel.output_string stdout (prompt ^ "\n\tErrors\n")) *)
(*     errors *)

let interact fname editors forms =
  let open Result.Monad_infix in
  let rec prompt_input () =
    write_forms fname forms;
    edit_forms ()
  and edit_forms () =
    run_editor fname editors >>= fun () ->
    read_input ()
  and read_input () =
    match read_forms fname forms with
      | Result.Ok inputs -> validate_inputs inputs
      | Result.Error `Bad_file -> begin
	Printf.printf "Unparsable file\n";
	prompt_input ()
      end
      | Result.Error (`Bad_prompt _) -> begin
	Printf.printf "Input contains an unknown prompt\n";
	prompt_input ()
      end
  and validate_inputs inputs =
    match validate_forms inputs forms with
      | Result.Ok valid_inputs ->
	Result.Ok valid_inputs
      | Result.Error errs -> begin
	print_errors forms errs;
	prompt_user ()
      end
  and prompt_user () =
    Printf.printf "Edit/Start over/Cancel (E/s/c): %!";
    match In_channel.input_line stdin with
      | Some input -> begin
	match input with
	  | "" | "E" | "e" ->
	    edit_forms ()
	  | "S" | "s" ->
	    prompt_input ()
	  | "C" | "c" ->
	    Result.Error `Cancelled
	  | _ ->
	    prompt_user ()
      end
      | None ->
	Result.Error `Cancelled
  in
  prompt_input ()

let run forms =
  let temp_file = Filename.temp_file "opass" "form" in
  let editors = get_editors () in
  let res = interact temp_file editors forms in
  Sys.remove temp_file;
  res
