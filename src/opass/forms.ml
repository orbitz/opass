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
  let module R = Db.Row in
  Editable_form.make [ Editable_entry.make
	                 ~name:"name"
	                 ~default:(Fn.const name)
	                 ~validate:not_empty
	                 ~prompt:"Name"
	             ; Editable_entry.make
	                 ~name:"location"
	                 ~default:(Fn.const p.R.location)
	                 ~validate:not_empty
	                 ~prompt:"Location"
	             ; Editable_entry.make
	                 ~name:"username"
	                 ~default:(Fn.const p.R.username)
	                 ~validate:anything
	                 ~prompt:"Username"
	             ; Editable_entry.make
	                 ~name:"password"
	                 ~default:(Fn.const p.R.password)
	                 ~validate:anything
	                 ~prompt:"Password (empty for random)"
	             ; Editable_entry.make
	                 ~name:"length"
	                 ~default:(Fn.const "24")
	                 ~validate:integer
	                 ~prompt:"Random password length"
	             ; Editable_entry.make
	                 ~name:"charset"
	                 ~default:(Fn.const "any")
	                 ~validate:charset
	                 ~prompt:"Random password charset (any/alpha/alphanum)"
	             ]

let note ?(name = "") ?(n = default_note) () =
  Editable_form.make [ Editable_entry.make
	                 ~name:"name"
	                 ~default:(Fn.const name)
	                 ~validate:not_empty
	                 ~prompt:"Name"
	             ; Editable_entry.make
	                 ~name:"note"
	                 ~default:(Fn.const n)
	                 ~validate:not_empty
	                 ~prompt:"Note"
	             ]
