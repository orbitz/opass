open Core.Std

type t

module Row : sig
  type name     = string

  type password = { host     : string
		  ; username : string
		  ; password : string
		  }

  type note     = string

  type t =
    | Password of (name * password)
    | Note     of (name * note)
end

val make      : unit -> t

val of_string : string -> t
val to_string : t -> string

val search : f:(Row.t -> bool) -> t -> Row.t list
