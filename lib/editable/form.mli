type t

val make : Entry.t list -> t
val validate : (string * string) list -> t -> (string * string list) list
val name_of_prompt : string -> t -> string option
val prompt_of_name : string -> t -> string option

val iter : f:(Entry.t -> unit) -> t -> unit

