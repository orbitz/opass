type t

type default = unit -> string
type validate = string -> string list option

val make : name:string -> default:default -> validate:validate -> prompt:string -> t

val validate : string -> t -> string list option
val prompt   : t -> string
val default  : t -> string
val name     : t -> string
