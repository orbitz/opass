open Core.Std

val read  : cmd:string -> string -> (Db.t, [`Bad_database]) Result.t
val write : Db.t -> cmd:string -> string -> (unit, exn) Result.t
