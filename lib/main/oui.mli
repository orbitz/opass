open Core.Std

val read_row : unit -> (Db.Row.t, [`Cancelled | `Bad_editor]) Result.t
val edit_row : Db.Row.t -> (Db.Row.t, [`Cancelled | `Bad_editor]) Result.t
