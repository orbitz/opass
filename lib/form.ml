open Core.Std

type t = Entry.t list

let make t = t

let rec validate values = function
  | [] ->
    []
  | e::es -> begin
    let value =
      List.Assoc.find
	values
	~equal:(=)
	(Entry.name e)
    in
    match value with
      | Some v -> begin
	match Entry.validate v e with
	  | Some errs ->
	    (Entry.name e, errs)::validate values es
	  | None ->
	    validate values es
      end
      | None ->
	(Entry.name e, ["Required"])::validate values es
  end

let name_of_prompt prompt t =
  Option.map
    (List.find ~f:(fun e -> Entry.prompt e = prompt) t)
    ~f:Entry.name

let prompt_of_name name t =
  Option.map
    (List.find ~f:(fun e -> Entry.name e = name) t)
    ~f:Entry.prompt

let to_list t = t
