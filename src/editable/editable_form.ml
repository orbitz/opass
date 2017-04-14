type t = Editable_entry.t list

let make t = t

let rec validate values = function
  | [] ->
    []
  | e::es -> begin
    let value =
      CCList.Assoc.get
        ~eq:(=)
        (Editable_entry.name e)
        values
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
  CCOpt.map
    Editable_entry.name
    (CCList.find_pred (fun e -> Editable_entry.prompt e = prompt) t)

let prompt_of_name name t =
  CCOpt.map
    Editable_entry.prompt
    (CCList.find_pred (fun e -> Editable_entry.name e = name) t)


let to_list t = t
