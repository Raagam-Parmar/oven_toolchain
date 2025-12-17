open Base
open Common.Bits

let translate_reg (r: Register.t) : Bits2.t =
  match r with
  | R0 -> Bits2.from_int 0b00
  | R1 -> Bits2.from_int 0b01
  | R2 -> Bits2.from_int 0b10
  | R3 -> Bits2.from_int 0b11


let translate_opcode inst : Bits4.t =
  match inst with
  | Load (_, _)  -> Bits4.from_int 0b0000
  | Store (_, _) -> Bits4.from_int 0b0001

  | Lui _        -> Bits4.from_int 0b0010
  | Lli _        -> Bits4.from_int 0b0011

  | Beqz (_, _)  -> Bits4.from_int 0b0100
  | Bltz (_, _)  -> Bits4.from_int 0b0101
  | Bgtz (_, _)  -> Bits4.from_int 0b0110
  | Jump _       -> Bits4.from_int 0b0111

  | Add (_, _)   -> Bits4.from_int 0b1000
  | Not _        -> Bits4.from_int 0b1001
  | And (_, _)   -> Bits4.from_int 0b1010
  | Or (_, _)    -> Bits4.from_int 0b1011
  | Sub (_, _)   -> Bits4.from_int 0b1100
  (* unused opcode 1101 *)
  | Move (_, _)  -> Bits4.from_int 0b1110
  | StPC _       -> Bits4.from_int 0b1111


let translate_instruction inst =
  match inst with
  | Lui imm
  | Lli imm ->
    concat_44
      imm
      (translate_opcode inst)

  | Load (rs1, rs2)
  | Store (rs1, rs2)
  | Beqz (rs1, rs2)
  | Bltz (rs1, rs2)
  | Bgtz (rs1, rs2)
  | Add (rs1, rs2)
  | And (rs1, rs2)
  | Or (rs1, rs2)
  | Sub (rs1, rs2)
  | Move (rs1, rs2) ->
    concat_224
      (translate_reg rs2)
      (translate_reg rs1)
      (translate_opcode inst)

  | Not rs1
  | StPC rs1 ->
    concat_224
      (Bits2.from_int 0b00)
      (translate_reg rs1)
      (translate_opcode inst)

  | Jump rs2 ->
    concat_224
      (translate_reg rs2)
      (Bits2.from_int 0b00)
      (translate_opcode inst)
