(* open Butter_sim.Cpu
open Butter_as
open Common.Utils

let random_a =
  "
    lli 3
    lui 10
    add r2 r0
    store r0 r1
    load r1 r1
    #stpc r3
    #jump r3
  "


let assembled = list_to_array (parse_and_assemble random_a)
let init_state = init assembled
let final_state = step_n init_state 200
let () = Butter_sim.Printer.pp_state final_state *)
