open Butter_isa

type ('num, 'lbl) instruction =
  | Load  of Register.t * Register.t
  | Store of Register.t * Register.t

  | Lui   of 'num
  | Lli   of 'num
  | Li    of 'num (* pseudo-instruction *)
  | La    of 'lbl (* pseudo-instruction *)

  | Move  of Register.t * Register.t
  | Add   of Register.t * Register.t
  | Sub   of Register.t * Register.t
  | And   of Register.t * Register.t
  | Or    of Register.t * Register.t
  | Not   of Register.t

  | Beqz  of Register.t * Register.t
  | Bltz  of Register.t * Register.t
  | Bgtz  of Register.t * Register.t
  | Bgez  of Register.t * Register.t (* pseudo-instruction *)
  | Blez  of Register.t * Register.t (* pseudo-instruction *)
  | Bnez  of Register.t * Register.t (* pseudo-instruction *)

  | Jump  of Register.t
  | StPC  of Register.t

  | Label of 'lbl (* pseudo-instruction *)

type ('num, 'lbl) program = ('num, 'lbl) instruction list
