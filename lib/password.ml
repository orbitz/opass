open Core.Std

type t = string

let lowercase = "abcdefghijklmnopqrstuvwxyz"
let alpha     = lowercase ^ String.uppercase lowercase
let num       = "0123456789"
let alphanum  = alpha ^ num
let symbols   = "!@#$%^&*();:<>,.?"
let all       = alphanum ^ symbols

let pick_n src len =
  let s   = String.create len in
  let s_l = String.length src in
  for i = 0 to len - 1 do
    String.set s i (String.get src (Random.int s_l))
  done;
  s

let mk_alpha = pick_n alpha

let mk_alphanum = pick_n alphanum

let mk_all = pick_n all

