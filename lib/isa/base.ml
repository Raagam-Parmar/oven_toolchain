open Common.Bits


type instruction =
  | Load  of Register.t * Register.t
  | Store of Register.t * Register.t

  | Lui   of Bits4.t
  | Lli   of Bits4.t

  | Move  of Register.t * Register.t
  | Add   of Register.t * Register.t
  | Sub   of Register.t * Register.t
  | And   of Register.t * Register.t
  | Or    of Register.t * Register.t
  | Not   of Register.t

  | Beqz  of Register.t * Register.t
  | Bltz  of Register.t * Register.t
  | Bgtz  of Register.t * Register.t

  | Jump  of Register.t
  | StPC  of Register.t


let to_string_RR inst rs1 rs2 =
  Printf.sprintf "%s\t%s %s"
    inst
    (Register.to_string rs1)
    (Register.to_string rs2)


let to_string_R inst r =
  Printf.sprintf "%s\t%s"
    inst
    (Register.to_string r)


let to_string_I inst imm =
  Printf.sprintf "%s\t%s"
    inst
    (Int.to_string (Bits4.to_int imm))


let to_string = function
  | Load (rs1, rs2) -> to_string_RR "load" rs1 rs2
  | Store (rs1, rs2) -> to_string_RR "store" rs1 rs2

  | Lui imm -> to_string_I "lui" imm
  | Lli imm -> to_string_I "lli" imm

  | Move (rs1, rs2) -> to_string_RR "mov" rs1 rs2
  | Add (rs1, rs2) -> to_string_RR "add" rs1 rs2
  | Sub (rs1, rs2) -> to_string_RR "sub" rs1 rs2
  | And (rs1, rs2) -> to_string_RR "and" rs1 rs2
  | Or (rs1, rs2) -> to_string_RR "or" rs1 rs2
  | Not rs1 -> to_string_R "not" rs1

  | Beqz (rs1, rs2) -> to_string_RR "beqz" rs1 rs2
  | Bltz (rs1, rs2) -> to_string_RR "bltz" rs1 rs2
  | Bgtz (rs1, rs2) -> to_string_RR "bgtz" rs1 rs2

  | Jump rs2 -> to_string_R "jump" rs2
  | StPC rs1 -> to_string_R "stpc" rs1
