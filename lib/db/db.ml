open Core.Std

module Row = struct
  type name     = string with sexp
  type password = { location : string
		  ; username : string
		  ; password : string
		  }
  with sexp

  type note     = string with sexp

  type elt =
    | Password of password
    | Note     of note
  with sexp

  type t = (name * elt) with sexp
end

type t = Row.t list with sexp

let make () =
  []

let of_string s =
  t_of_sexp (Sexp.of_string s)

let to_string t =
  Sexp.to_string_hum (sexp_of_t t)

let search ~f t =
  List.filter t ~f

let add ((name, _) as row) t =
  let f = Fn.compose ((=) name) fst in
  (* Find dups *)
  match search ~f t with
    | [] ->
      Result.Ok (row::t)
    | _::_ ->
      Result.Error `Duplicate

let delete name t =
  let f = Fn.compose ((<>) name) fst in
  search ~f t

let of_rows rows =
  let open Result.Monad_infix in
  let rec f t = function
    | [] -> Result.Ok t
    | r::rs ->
      add r t >>= (Fn.flip f rs)
  in
  f (make ()) rows

let rec merge t = function
  | [] ->
    Result.Ok t
  | r::rs ->
    let open Result.Monad_infix in
    add r t >>= (Fn.flip merge rs)
