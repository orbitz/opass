open Core.Std

type t = Entry.t list

let make fields = fields

let rec validate values = function
  | [] ->
    []
  | f::fs -> begin
    let value =
      List.Assoc.find
	values
	~equal:(=)
	(Entry.name f)
    in
    match value with
      | Some v -> begin
	match Entry.validate v f with
	  | Some errs ->
	    (Entry.name f, errs)::validate values fs
	  | None ->
	    validate values fs
      end
      | None ->
	(Entry.name f, ["Required"])::validate values fs
  end

let name_of_prompt prompt t =
  Option.map
    (List.find ~f:(fun f -> Entry.prompt f = prompt) t)
    ~f:Entry.name

let prompt_of_name name t =
  Option.map
    (List.find ~f:(fun f -> Entry.name f = name) t)
    ~f:Entry.prompt

let iter ~f t =
  List.iter ~f t

