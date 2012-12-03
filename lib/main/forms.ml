open Core.Std

let not_empty = function
  | "" -> Some ["Cannot be empty"]
  | _  -> None

let anything = Fn.const None

let integer s =
  try
    ignore (Int.of_string s);
    None
  with
      _ -> Some ["Must be an integer"]

let charset = function
  | "any" | "alpha" | "alphanum" -> None
  | s -> Some [s ^ " is an invalid charset"; "Must be any, alpha, or alphanum"]

let default_password =
  let module R = Db.Row in
  { R.location = ""
  ;   username = ""
  ;   password = ""
  }

let default_note =
  ""

let password ?(name = "") ?(p = default_password) () =
  let open Editable in
  let module R = Db.Row in
  Form.make [ Entry.make
	      ~name:"name"
	      ~default:(Fn.const name)
	      ~validate:not_empty
	      ~prompt:"Name"
	    ; Entry.make
	      ~name:"location"
	      ~default:(Fn.const p.R.location)
	      ~validate:not_empty
	      ~prompt:"Location"
	    ; Entry.make
	      ~name:"username"
	      ~default:(Fn.const p.R.username)
	      ~validate:anything
	      ~prompt:"Username"
	    ; Entry.make
	      ~name:"password"
	      ~default:(Fn.const p.R.password)
	      ~validate:anything
	      ~prompt:"Password (empty for random)"
	    ; Entry.make
	      ~name:"length"
	      ~default:(Fn.const "24")
	      ~validate:integer
	      ~prompt:"Random password length"
	    ; Entry.make
	      ~name:"charset"
	      ~default:(Fn.const "any")
	      ~validate:charset
	      ~prompt:"Random password charset (any/alpha/alphanum)"
	    ]

let note ?(name = "") ?(n = default_note) () =
  let open Editable in
  Form.make [ Entry.make
	      ~name:"name"
	      ~default:(Fn.const name)
	      ~validate:not_empty
	      ~prompt:"Name"
	    ; Entry.make
	      ~name:"note"
	      ~default:(Fn.const default_note)
	      ~validate:not_empty
	      ~prompt:"Note"
	    ]
