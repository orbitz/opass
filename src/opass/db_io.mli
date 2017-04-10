val read  : cmd:string -> string -> (Db.t, [> `Bad_database]) result
val write : Db.t -> cmd:string -> string -> (unit, exn) result
