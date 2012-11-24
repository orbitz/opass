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

let password =
  let open Editable in
  Form.make [ Entry.make
	      ~name:"name"
	      ~default:(Fn.const "")
	      ~validate:not_empty
	      ~prompt:"Name"
	    ; Entry.make
	      ~name:"location"
	      ~default:(Fn.const "")
	      ~validate:not_empty
	      ~prompt:"Location"
	    ; Entry.make
	      ~name:"username"
	      ~default:(Fn.const "")
	      ~validate:anything
	      ~prompt:"Username"
	    ; Entry.make
	      ~name:"password"
	      ~default:(Fn.const "")
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

let note =
  let open Editable in
  Form.make [ Entry.make
	      ~name:"name"
	      ~default:(Fn.const "")
	      ~validate:not_empty
	      ~prompt:"Name"
	    ; Entry.make
	      ~name:"note"
	      ~default:(Fn.const "")
	      ~validate:not_empty
	      ~prompt:"Note"
	    ]
