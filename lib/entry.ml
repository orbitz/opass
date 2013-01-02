type default = unit -> string
type validate = string -> string list option

type t = { name     : string
	 ; default  : default
	 ; validate : validate
	 ; prompt   : string
	 }


let make ~name ~default ~validate ~prompt =
  { name; default; validate; prompt }

let validate v t =
  t.validate v

let prompt t =
  t.prompt

let default t =
  t.default ()

let name t =
  t.name
