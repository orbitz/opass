let not_empty = function
  | "" -> Some ["Cannot be empty"]
  | _  -> None

let anything = CCFun.const None

let integer s =
  try
    ignore (int_of_string s);
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
	                 ~default:(CCFun.const name)
	                 ~validate:not_empty
	                 ~prompt:"Name"
	             ; Editable_entry.make
	                 ~name:"location"
	                 ~default:(CCFun.const p.R.location)
	                 ~validate:not_empty
	                 ~prompt:"Location"
	             ; Editable_entry.make
	                 ~name:"username"
	                 ~default:(CCFun.const p.R.username)
	                 ~validate:anything
	                 ~prompt:"Username"
	             ; Editable_entry.make
	                 ~name:"password"
	                 ~default:(CCFun.const p.R.password)
	                 ~validate:anything
	                 ~prompt:"Password (empty for random)"
	             ; Editable_entry.make
	                 ~name:"length"
	                 ~default:(CCFun.const "24")
	                 ~validate:integer
	                 ~prompt:"Random password length"
	             ; Editable_entry.make
	                 ~name:"charset"
	                 ~default:(CCFun.const "any")
	                 ~validate:charset
	                 ~prompt:"Random password charset (any/alpha/alphanum)"
	             ]

let note ?(name = "") ?(n = default_note) () =
  Editable_form.make [ Editable_entry.make
	                 ~name:"name"
	                 ~default:(CCFun.const name)
	                 ~validate:not_empty
	                 ~prompt:"Name"
	             ; Editable_entry.make
	                 ~name:"note"
	                 ~default:(CCFun.const n)
	                 ~validate:not_empty
	                 ~prompt:"Note"
	             ]
