open Core.Std

type t

module Row : sig
  type name     = string
  type password = { location : string
                  ; username : string
                  ; password : string
                  }

  type note     = string

  type elt =
    | Password of password
    | Note     of note

  type t = (name * elt)
end

val make      : unit -> t

val of_string : string -> t
val to_string : t -> string

val of_rows   : Row.t list -> (t, [> `Duplicate]) Result.t

val search : f:(Row.t -> bool) -> t -> Row.t list

val add    : Row.t -> t -> (t, [> `Duplicate]) Result.t
val delete : Row.name -> t -> t

val merge  : t -> t -> (t, [> `Duplicate]) Result.t
