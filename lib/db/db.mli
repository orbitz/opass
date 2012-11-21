open Core.Std

type t

module Add_error : sig
  type t =
    | Duplicate
end

module Row : sig
  type name     = string with sexp
  type password = { host     : string
		  ; username : string
		  ; password : string
		  }
  with sexp

  type note     = string with sexp

  type elt =
    | Password of password
    | Note     of note
  with sexp

  type t = (name * elt)
end

val make      : unit -> t

val of_string : string -> t
val to_string : t -> string

val search : f:(Row.t -> bool) -> t -> Row.t list

val add    : Row.t -> t -> (t, Add_error.t) Result.t
val delete : Row.name -> t -> t
