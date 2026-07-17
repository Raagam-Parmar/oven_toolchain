(** [make_in_channel fp] returns in-channel for [fp] if it exists, otherwise defaults
    to a file called `input.asm' in the dune directory and returns its in-channel. *)
let mk_in_ch file_path =
  try Some (open_in file_path)
  with Sys_error _ ->
    Printf.eprintf "Error opening file.\n";
    None


(** [read_file ic] reads contents of in-channel [ic] as a string and closes it. *)
let read_file ic =
  let buffer = Buffer.create 4096 in
  try
    while true do
      Buffer.add_channel buffer ic 4096
    done;
    ""
  with End_of_file ->
    close_in_noerr ic;
    Buffer.contents buffer


let usage = "Usage: butter_as <filename.btr>"


(* main program *)

let () =
  if Array.length Sys.argv != 2
  then (
    print_endline usage;
    exit 1
  )
  else
    let input = Sys.argv.(1) in

    match mk_in_ch input with
    | Some in_ch ->
      in_ch
      |> read_file
      |> Butter_as.parse_and_assemble
      |> Common.Utils.list_to_array
      |> Butter_sim.Cpu.init
      |> Butter_debugger.Debugger.repl

    | None       -> exit 2
