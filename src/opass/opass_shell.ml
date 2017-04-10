let sh ~input cmd =
  let (ic, oc) = Unix.open_process cmd in
  output_string oc input;
  close_out oc;
  let input = CCIO.read_all ic in
  close_in ic;
  input
