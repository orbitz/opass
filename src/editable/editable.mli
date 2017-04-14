type errors = [ `Cancelled | `Bad_editor ]

val run : Editable_form.t list -> ((string * string) list list, errors) result
