open Core.Std

type errors = [ `Cancelled | `Bad_editor ]

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

let write_form fname form =
  Out_channel.with_file
    fname
    ~f:(fun fout ->
      Form.iter
	~f:(fun f ->
	  Out_channel.output_string
	    fout
	    (Entry.prompt f ^ ": " ^ Entry.default f ^ "\n"))
	form)

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
	    Result.Error (`Bad_prompt p)
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

let read_form fname form =
  let lines =
    In_channel.with_file
      fname
      ~f:In_channel.input_lines
  in
  read_first_line form lines

let validate_form input form =
  match Form.validate input form with
    | [] ->
      Result.Ok input
    | errors ->
      Result.Error errors

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

let print_errors form errors =
  List.iter
    ~f:(fun (n, _) ->
      let prompt =
	match Form.prompt_of_name n form with
	  | Some p -> p
	  | None   -> "Unknown"
      in
      Out_channel.output_string stdout (prompt ^ "\n\tErrors\n"))
    errors

let interact fname editors form =
  let open Result.Monad_infix in
  let rec prompt_input () =
    write_form fname form;
    edit_form ()
  and edit_form () =
    run_editor fname editors >>= fun () ->
    read_input ()
  and read_input () =
    match read_form fname form with
      | Result.Ok input -> validate_input input
      | Result.Error `Bad_file -> prompt_input ()
      | Result.Error (`Bad_prompt _) -> prompt_input ()
  and validate_input input =
    match validate_form input form with
      | Result.Ok valid_input ->
	Result.Ok valid_input
      | Result.Error errs -> begin
	print_errors form errs;
	prompt_user ()
      end
  and prompt_user () =
    Printf.printf "Edit/Start over/Cancel (E/s/c): %!";
    match In_channel.input_line stdin with
      | Some input -> begin
	match input with
	  | "" | "E" | "e" ->
	    edit_form ()
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

let run form =
  let temp_file = Filename.temp_file "opass" "form" in
  let editors = get_editors () in
  let res = interact temp_file editors form in
  Sys.remove temp_file;
  res
