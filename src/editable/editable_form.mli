type t

val make : Editable_entry.t list -> t
val validate : (string * string) list -> t -> (string * string list) list
val name_of_prompt : string -> t -> string option
val prompt_of_name : string -> t -> string option

val to_list : t -> Editable_entry.t list
