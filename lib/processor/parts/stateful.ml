open Types
open Basics


type register_input = {inp: bit; load: bit}
type register_state = bit
type register_output = bit
let register
    (ip_st: register_input * register_state)
  : register_output * register_state
  =
  let ip = fst ip_st in
  let state = snd ip_st in
  let mux_out = mux2 {a= state; b=ip.inp; addr=ip.load} in
  (state, mux_out)


type register_x8_input = {inp: bit8; load: bit}
type register_x8_state = bit8
type register_x8_output = bit8
let register_x8
    (ip_st: register_x8_input * register_x8_state)
  : register_x8_output * register_x8_state
  =
  let ip = fst ip_st in
  let state = snd ip_st in
  let s7, s6, s5, s4, s3, s2, s1, s0 = state in
  let i7, i6, i5, i4, i3, i2, i1, i0 = ip.inp in
  let o7, s7' = register ({inp=i7; load=ip.load}, s7) in
  let o6, s6' = register ({inp=i6; load=ip.load}, s6) in
  let o5, s5' = register ({inp=i5; load=ip.load}, s5) in
  let o4, s4' = register ({inp=i4; load=ip.load}, s4) in
  let o3, s3' = register ({inp=i3; load=ip.load}, s3) in
  let o2, s2' = register ({inp=i2; load=ip.load}, s2) in
  let o1, s1' = register ({inp=i1; load=ip.load}, s1) in
  let o0, s0' = register ({inp=i0; load=ip.load}, s0) in
  let out = (o7, o6, o5, o4, o3, o2, o1, o0) in
  let state' = (s7', s6', s5', s4', s3', s2', s1', s0') in
  (out, state')
