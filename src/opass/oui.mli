val read_row : unit -> (Db.Row.t, [> `Cancelled | `Bad_editor]) result
val edit_row : Db.Row.t -> (Db.Row.t, [> `Cancelled | `Bad_editor]) result
