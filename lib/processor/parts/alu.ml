open Types
open Basics
open Adders
open Common.Bits.Bit


(*
  | input | zr | ng | ps |
  |-------|----|----|----|
  | zero  |  1 | 0  |  1 |
  | 1***  |  0 | 1  |  0 |
  | 0***  |  0 | 0  |  1 |
*)
type comparator_input = bit8
type comparator_output = {zr: bit; ng: bit; ps: bit}
let comparator ip =
  let not_zero = orr_8way ip in
  let zero = nott not_zero in
  let negative, _, _, _, _, _, _, _ = ip in
  let positive = nott negative in
  {zr=zero; ng=negative; ps=positive}


(*
  | zext | operation |
  |------|-----------|
  |   0  | imm << 4  |
  |   1  | zext(imm) |
*)
type immgen_input = {imm: bit4; zext: bit}
type immgen_output = bit8
let immgen (ip: immgen_input) : immgen_output =
  let im3, im2, im1, im0 = ip.imm in
  let zext_out = (O, O, O, O, im3, im2, im1, im0) in
  let shift_out = (im3, im2, im1, im0, O, O, O, O) in
  let out = mux2_x8 {a=shift_out; b=zext_out; addr=ip.zext} in
  out


(*
  | sub | operation |
  |-----|-----------|
  |  0  |    add    |
  |  1  |    sub    |
*)
type addsub_unit_input = {a: bit8; b: bit8; sub: bit}
type addsub_unit_output = bit8
let addsub_unit (ip: addsub_unit_input) : addsub_unit_output =
  let aux = fanout_1_8 ip.sub in
  let b' = xor_x8 {a=aux; b=ip.b} in
  let sum = adder_x8 {a=ip.a; b=b'} in
  sum


type alu_input = {a: bit8; b: bit8; ctrl: bit3}
type alu_output = bit8
(*
  | ctrl[2:0] | operation |
  |---|---|---|-----------|
  | 0 | 0 | 0 |    add    |
  | 0 | 0 | 1 |    not    |
  | 0 | 1 | 0 |    and    |
  | 0 | 1 | 1 |    or     |
  | 1 | 0 | 0 |    sub    |
  | 1 | 0 | 1 |           |
  | 1 | 1 | 0 |    pas    |
  | 1 | 1 | 1 |    pas    |
*)
let alu ip =
  let c2, c1, c0 = ip.ctrl in
  let not_zero_b_ctrl = andd_3way ip.ctrl in
  let zero_b_ctrl = nott not_zero_b_ctrl in
  let false8 = fanout_1_8 zero_b_ctrl in
  let b' = andd_x8 {a=ip.b; b=false8} in
  let addsub_out = addsub_unit {a=ip.a; b=b'; sub=c2} in
  let not_out = not_x8 ip.a in
  let and_out = andd_x8 {a=ip.a; b=b'} in
  let or_out = orr_x8 {a=ip.a; b=b'} in
  let out =
    mux4_x8 {
      a=addsub_out;
      b=not_out;
      c=and_out;
      d=or_out;
      addr=(c1, c0)
    }
  in
  out
