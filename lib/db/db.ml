open Core.Std

module Row = struct
  type name     = string with sexp

  type password = { host     : string
		  ; username : string
		  ; password : string
		  }
  with sexp

  type note     = string with sexp

  type t =
    | Password of (name * password)
    | Note     of (name * note)
  with sexp
end

type t = Row.t list with sexp

let of_string s =
  t_of_sexp (Sexp.of_string s)

let to_string t =
  Sexp.to_string_hum (sexp_of_t t)

let search ~f t =
  List.filter t ~f
