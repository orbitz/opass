open Core.Std

let sh ~input cmd =
  let (ic, oc) = Unix.open_process cmd in
  Out_channel.output_string oc input;
  Out_channel.close oc;
  let input = In_channel.input_all ic in
  In_channel.close ic;
  input
