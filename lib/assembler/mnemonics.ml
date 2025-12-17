open Ast
open Common.Bits
open Common.Utils

exception LoadImmediateOutOfBounds of int

(* Load Store shorthands *)
let load  rs1 rs2 = [ Load (rs1, rs2) ]
let store rs1 rs2 = [ Store (rs1, rs2) ]


(* Load Immediate shorthands *)
let lli n = [ Lli (Bits4.from_int n) ]
let lui n = [ Lui (Bits4.from_int n) ]

(** Load arbitrary 8-bit number,
    expands into one or two base instructions *)
let li n =
  if not (in_range ~-128 127 n)
  then raise (LoadImmediateOutOfBounds n)
  else
    let n_lo = n mod 16 in
    let n_hi = (Int.shift_right n 4) in
    if n_hi = 0 then lli n_lo
    else lli n_lo @ lui n_hi


(* Basic Arithmetic shorthands *)
let mov  rs1 rs2 = [ Move (rs1, rs2) ]
let add  rs1 rs2 = [ Add (rs1, rs2) ]
let sub  rs1 rs2 = [ Sub (rs1, rs2) ]
let andd rs1 rs2 = [ And (rs1, rs2) ]
let orr  rs1 rs2 = [ Or (rs1, rs2) ]
let nott rs1     = [ Not rs1 ]


(* Conditional Branch shorthands *)
let beqz rs1 rs2 = [ Beqz (rs1, rs2) ]
let bltz rs1 rs2 = [ Bltz (rs1, rs2) ]
let bgtz rs1 rs2 = [ Bgtz (rs1, rs2) ]
let blez rs1 rs2 = bltz rs1 rs2 @ beqz rs1 rs2
let bgez rs1 rs2 = bgtz rs1 rs2 @ beqz rs1 rs2
let bnez rs1 rs2 = bltz rs1 rs2 @ bgtz rs1 rs2

(* PC-Direct shorthands *)
let jump rs2 = [ Jump rs2 ]
let stpc rs1 = [ StPC rs1 ]
