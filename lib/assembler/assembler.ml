open Ast
open Mnemonics
open Butter_isa

exception RepeatedLabel of string
exception UnknownLabel of string
exception UnexpandedInstruction

module SymTbl = Map.Make(String)

(** Expand pseudo-instruction to base instruction(s) *)
let expand_i instruction  =
  match instruction with
  | Load  (rs1, rs2) -> load rs1 rs2
  | Store (rs1, rs2) -> store rs1 rs2

  | Lui imm  -> lui imm
  | Lli imm  -> lli imm
  | Li  imm  -> li imm
  | La  lbl  -> [ La lbl ]

  | Move (rs1, rs2) -> mov rs1 rs2
  | Add  (rs1 ,rs2) -> add rs1 rs2
  | Sub  (rs1, rs2) -> sub rs1 rs2
  | And  (rs1, rs2) -> andd rs1 rs2
  | Or   (rs1, rs2) -> orr rs1 rs2
  | Not   rs1       -> nott rs1

  | Beqz (rs1, rs2) -> beqz rs1 rs2
  | Bltz (rs1, rs2) -> bltz rs1 rs2
  | Bgez (rs1, rs2) -> bgez rs1 rs2
  | Bgtz (rs1, rs2) -> bgtz rs1 rs2
  | Blez (rs1, rs2) -> blez rs1 rs2
  | Bnez (rs1, rs2) -> bnez rs1 rs2

  | Jump       rs2  -> jump rs2
  | StPC  rs1       -> stpc rs1

  | Label lbl -> [ Label lbl ]


(** Expand all pseudi-instructions in a program *)
let expand_p program =
  List.concat (List.map expand_i program)


let add_symbol_i symtbl n instruction =
  match instruction with
  | Label lbl ->
    if SymTbl.mem lbl symtbl
    then raise (RepeatedLabel lbl)
    else
      SymTbl.add lbl n symtbl

  | _ -> symtbl


let rec add_symbol_p symtbl program n =
  match program with
  | [] -> symtbl
  | i :: is ->
    let symtbl' = add_symbol_i symtbl n i in
    add_symbol_p symtbl' is (n + 1)


let populate_symtbl symtbl program =
  add_symbol_p symtbl program 0


let resolve_label_i symtbl instruction =
  match instruction with
  | La lbl ->
    let line_no =
      match SymTbl.find_opt lbl symtbl with
      | None -> raise (UnknownLabel lbl)
      | Some n -> n
    in
    li line_no

  | Label _ -> []
  | _ -> [ instruction ]


let resolve_label_p symtbl program =
  List.concat (List.map (resolve_label_i symtbl) program)


let reduce_i instruction =
  match instruction with
  | Load  (rs1, rs2) -> Base.Load  (rs1, rs2)
  | Store (rs1, rs2) -> Base.Store (rs1, rs2)

  | Lui imm  -> Base.Lui imm
  | Lli imm  -> Base.Lli imm

  | Move (rs1, rs2) -> Base.Move (rs1, rs2)
  | Add  (rs1 ,rs2) -> Base.Add  (rs1 ,rs2)
  | Sub  (rs1, rs2) -> Base.Sub  (rs1, rs2)
  | And  (rs1, rs2) -> Base.And  (rs1, rs2)
  | Or   (rs1, rs2) -> Base.Or   (rs1, rs2)
  | Not   rs1       -> Base.Not   rs1

  | Beqz (rs1, rs2) -> Base.Beqz (rs1, rs2)
  | Bltz (rs1, rs2) -> Base.Bltz (rs1, rs2)
  | Bgtz (rs1, rs2) -> Base.Bgtz (rs1, rs2)

  | Jump       rs2  -> Base.Jump       rs2
  | StPC  rs1       -> Base.StPC  rs1

  | Li _ | La _
  | Bgez _ | Blez _ | Bnez _
  | Label _ -> raise UnexpandedInstruction


let reduce_p program =
  List.map reduce_i program


let parse s =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.main Lexer.read lexbuf in
  ast


let assemble program =
  let expanded_program = expand_p program in
  let empty = SymTbl.empty in
  let symtbl = populate_symtbl empty expanded_program in
  let resolved_program = resolve_label_p symtbl expanded_program in
  let reduced_program = reduce_p resolved_program in
  reduced_program


let translate program =
  List.map
    Translation.translate_instruction
    program


let parse_and_assemble program =
  program
  |> parse
  |> assemble


let parse_assemble_translate program =
  program
  |> parse
  |> assemble
  |> translate
