open Core.Std

type errors = [ `Cancelled | `Bad_editor ]

val run : Form.t -> ((string * string) list, errors) Result.t
