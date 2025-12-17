open Butter_isa.Base
open Common.Bits
open Common.Utils

type step_error =
  | PCOutOfBounds of Bits8.t


let report_step_error = function
  | PCOutOfBounds pc ->
    Printf.printf
      "Error: Program counter %s is pointing outside of the program range.\n"
      (Bits8.to_string pc)


type 'a state =
  { rom: instruction Ram.t;
    length: int;
    ram: 'a Ram.t;
    regfile: 'a Regfile.t;
    pc: 'a;
    dpage: 'a;
    ipage: 'a
  }


(** Initial program state *)
let init program =
  { rom = program;
    length = Array.length program;
    ram = Ram.create 256 Bits8.zero;
    regfile = Regfile.create Bits8.zero;
    pc = Bits8.zero;
    dpage = Bits8.zero;
    ipage = Bits8.zero
  }


(** Given instruction to execute, step through one cycle *)
let stepi state instruction =
  let ram = state.ram in
  let regfile = state.regfile in
  let pc = state.pc in

  match instruction with
  | Load (rs1, rs2) ->
    let vrs2 = Regfile.read regfile rs2 in
    let addr = Bits8.to_int vrs2 in
    let data = Ram.read ram addr in
    let () = Regfile.write regfile rs1 data in
    { state with pc = Bits8.succ pc }

  | Store (rs1, rs2) ->
    let vrs2 = Regfile.read regfile rs2 in
    let addr = Bits8.to_int vrs2 in
    let vrs1 = Regfile.read regfile rs1 in
    let () = Ram.write ram addr vrs1 in
    { state with pc = Bits8.succ pc }

  | Lui imm4 ->
    let imm8 =
      Bits8.from_int
        (Int.shift_left (Bits4.to_int imm4) 4)
    in
    let vr0_old = Regfile.read regfile R0 in
    let vr0_new = Bits8.logor imm8 vr0_old in
    let () = Regfile.write regfile R0 vr0_new in
    { state with pc = Bits8.succ pc }

  | Lli imm4 ->
    let imm8 =
      Bits8.from_int
        (Bits4.to_int imm4)
    in
    let () = Regfile.write regfile R0 imm8 in
    { state with pc = Bits8.succ pc }

  | Move (rs1, rs2) ->
    let vrs2 = Regfile.read regfile rs2 in
    let () = Regfile.write regfile rs1 vrs2 in
    { state with pc = Bits8.succ pc }

  | Add (rs1, rs2) ->
    let vrs1 = Regfile.read regfile rs1 in
    let vrs2 = Regfile.read regfile rs2 in
    let vsum = Bits8.add vrs1 vrs2 in
    let () = Regfile.write regfile rs1 vsum in
    { state with pc = Bits8.succ pc }

  | Sub (rs1, rs2) ->
    let vrs1 = Regfile.read regfile rs1 in
    let vrs2 = Regfile.read regfile rs2 in
    let vdiff = Bits8.sub vrs1 vrs2 in
    let () = Regfile.write regfile rs1 vdiff in
    { state with pc = Bits8.succ pc }

  | And (rs1, rs2) ->
    let vrs1 = Regfile.read regfile rs1 in
    let vrs2 = Regfile.read regfile rs2 in
    let vand = Bits8.logand vrs1 vrs2 in
    let () = Regfile.write regfile rs1 vand in
    { state with pc = Bits8.succ pc }

  | Or (rs1, rs2) ->
    let vrs1 = Regfile.read regfile rs1 in
    let vrs2 = Regfile.read regfile rs2 in
    let vor = Bits8.logor vrs1 vrs2 in
    let () = Regfile.write regfile rs1 vor in
    { state with pc = Bits8.succ pc }

  | Not rs1 ->
    let vrs1 = Regfile.read regfile rs1 in
    let vnot = Bits8.lognot vrs1 in
    let () = Regfile.write regfile rs1 vnot in
    { state with pc = Bits8.succ pc }

  | Beqz (rs1, rs2) ->
    let vrs1 = Regfile.read regfile rs1 in
    let vrs2 = Regfile.read regfile rs2 in
    let pc' =
      if vrs1 = Bits8.zero then vrs2
      else Bits8.succ pc
    in
    { state with pc = pc' }

  | Bltz (rs1, rs2) ->
    let vrs1 = Regfile.read regfile rs1 in
    let vrs2 = Regfile.read regfile rs2 in
    let pc' =
      if vrs1 < Bits8.zero then vrs2
      else Bits8.succ pc
    in
    { state with pc = pc' }

  | Bgtz (rs1, rs2) ->
    let vrs1 = Regfile.read regfile rs1 in
    let vrs2 = Regfile.read regfile rs2 in
    let pc' =
      if vrs1 < Bits8.zero then vrs2
      else Bits8.succ pc
    in
    { state with pc = pc' }

  | Jump rs2 ->
    let vrs2 = Regfile.read regfile rs2 in
    let pc' = vrs2 in
    { state with pc = pc' }

  | StPC rs1 ->
    let () = Regfile.write regfile rs1 pc in
    { state with pc = Bits8.succ pc }


(** Step through one cycle *)
let step state =
  let pc = state.pc in
  let int_pc = Bits8.to_int pc in
  if not (in_range 0 (state.length - 1) int_pc)
  then Error (PCOutOfBounds pc)
  else
    let inst = state.rom.(int_pc) in
    let state' = stepi state inst in
    Ok state'


(** Step through [n] cycles *)
let rec step_n state n =
  if n = 0 then (state, None)
  else
    match step state with
    | Ok state' -> step_n state' (n - 1)
    | Error e -> (state, Some e)
