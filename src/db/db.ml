open Sexplib
open Sexp
open Conv

module Row = struct
  type name     = string [@@deriving sexp]
  type password = { location : string
                  ; username : string
                  ; password : string
                  } [@@deriving sexp]

  type note     = string [@@deriving sexp]

  type elt =
    | Password of password
    | Note     of note
  [@@deriving sexp]

  type t = (name * elt) [@@deriving sexp]
end

type t = Row.t list [@@deriving sexp]

let make () =
  []

let of_string s =
  t_of_sexp (Sexplib.Sexp.of_string s)

let to_string t =
  Sexplib.Sexp.to_string_hum (sexp_of_t t)

let search ~f t =
  CCListLabels.filter t ~f

let add ((name, _) as row) t =
  let f = CCFun.compose fst ((=) name) in
  (* Find dups *)
  match search ~f t with
    | [] ->
      Ok (row::t)
    | _::_ ->
      Error `Duplicate

let delete name t =
  let f = CCFun.compose fst ((<>) name) in
  search ~f t

let of_rows rows =
  let open CCResult.Infix in
  let rec f t = function
    | [] -> Ok t
    | r::rs ->
      add r t
      >>= (CCFun.flip f rs)
  in
  f (make ()) rows

let rec merge t = function
  | [] ->
    Ok t
  | r::rs ->
    let open CCResult.Infix in
    add r t
    >>= (CCFun.flip merge rs)
