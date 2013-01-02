open Core.Std

type errors = [ `Cancelled | `Bad_editor ]

val run : Form.t list -> ((string * string) list list, errors) Result.t
