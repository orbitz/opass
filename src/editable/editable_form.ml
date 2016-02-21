open Core.Std

type t = Editable_entry.t list

let make t = t

let rec validate values = function
  | [] ->
    []
  | e::es -> begin
    let value =
      List.Assoc.find
	values
	~equal:(=)
	(Editable_entry.name e)
    in
    match value with
      | Some v -> begin
	match Editable_entry.validate v e with
	  | Some errs ->
	    (Editable_entry.name e, errs)::validate values es
	  | None ->
	    validate values es
      end
      | None ->
	(Editable_entry.name e, ["Required"])::validate values es
  end

let name_of_prompt prompt t =
  Option.map
    (List.find ~f:(fun e -> Editable_entry.prompt e = prompt) t)
    ~f:Editable_entry.name

let prompt_of_name name t =
  Option.map
    (List.find ~f:(fun e -> Editable_entry.name e = name) t)
    ~f:Editable_entry.prompt

let to_list t = t
