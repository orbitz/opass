open Core.Std

val read_row : unit -> (Db.Row.t, [`Cancelled | `Bad_editor]) Result.t
