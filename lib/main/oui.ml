open Core.Std

module Shell = Core_extended.Std.Shell

type line =
  | New      of (string * string)
  | Continue of string


let p = Printf.printf

let password_template =
  [ ("Name", "name")
  ; ("Host", "host")
  ; ("Password (empty for random)", "password")
  ; ("Random password length (default 24)", "length")
  ; ("all/alpha/alphanum (default all)", "charset")
  ]

let key_find k l =
  List.Assoc.find_exn l ~equal:(=) k

let write_template fname template =
  let fout = open_out fname in
  List.iter
    ~f:(fun (v, _) -> Out_channel.output_string fout (v ^ ": \n"))
    template;
  Out_channel.close fout

let rec parse_lines template (ch, cl) acc = function
  | [] ->
    (ch, cl)::acc
  | x::xs -> begin
    match String.lsplit2 ~on:':' x with
      | Some (h, l) ->
	let h = key_find h template in
	parse_lines template (h, String.strip l) ((ch, cl)::acc) xs
      | None ->
	parse_lines template (ch, cl ^ String.strip x) acc xs
  end

let parse_first_line template = function
  | [] ->
    failwith "Empty file"
  | x::xs -> begin
    match String.lsplit2 ~on:':' x with
      | Some (h, l) -> begin
	let h = key_find h template in
	parse_lines template (h, String.strip l) [] xs
      end
      | None ->
	failwith "Should never get here"
  end

let parse_file fname template =
  let fin = open_in fname in
  let lines = In_channel.input_lines fin in
  In_channel.close fin;
  parse_first_line template lines

let read_template template =
  match Sys.getenv "EDITOR" with
    | Some editor -> begin
      let temp_file = Filename.temp_file "opass" "password" in
      write_template temp_file template;
      ignore (Unix.system (editor ^ " " ^ temp_file));
      let m = parse_file temp_file template in
      Sys.remove temp_file;
      m
    end
    | None ->
      failwith "No editor set"

let read_password () =
  let m = read_template password_template in
  List.iter
    ~f:(fun (d, k) ->
      p "%s: %s\n" d (key_find k m))
    password_template;
  p "Is this correct? (Y/n): ";
  failwith "Done"

let read_note () =
  failwith "note"

let rec read_row_option () =
  let open Option.Monad_infix in
  p "Password/note (P/n): %!";
  In_channel.input_line stdin >>= function
    | "" | "P" | "p" ->
      read_password ()
    | "N" | "n" ->
      read_note ()
    | _ -> begin
      p "Unknown input, try again\n";
      read_row_option ()
    end

let read_row () =
  match read_row_option () with
    | Some row ->
      Result.Ok row
    | None ->
      failwith "Could not read row"

