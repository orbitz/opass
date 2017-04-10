type t = string

let lowercase = "abcdefghijklmnopqrstuvwxyz"
let alpha     = lowercase ^ CCString.uppercase_ascii lowercase
let num       = "0123456789"
let alphanum  = alpha ^ num
let symbols   = "!@#$%^&*();:<>,.?"
let all       = alphanum ^ symbols

let pick_n src len =
  let s   = Bytes.create len in
  let s_l = Bytes.length src in
  for i = 0 to len - 1 do
    Bytes.set s i (String.get src (Random.int s_l))
  done;
  Bytes.to_string s

let mk_alpha = pick_n alpha

let mk_alphanum = pick_n alphanum

let mk_all = pick_n all

