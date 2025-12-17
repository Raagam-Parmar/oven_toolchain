%{
  open Ast
%}

%token EOF

%token LOAD
%token STORE

%token LUI
%token LLI
%token LI
%token LA

%token MOV
%token ADD
%token SUB
%token AND
%token OR
%token NOT

%token BEQZ
%token BLTZ
%token BGEZ
%token BGTZ
%token BLEZ
%token BNEZ

%token JUMP
%token STPC

%token <Butter_isa.Register.t> REG
%token <int> IMM

%token COLON
%token <string> LABEL

%start <(int, string) Ast.program> main

%%

let main :=
  | is=instruction*; EOF;     { is }

let instruction :=
  | LOAD ; rs1=REG; rs2=REG;  { Load  (rs1, rs2) }
  | STORE; rs1=REG; rs2=REG;  { Store (rs1, rs2) }

  | LUI  ;          imm=IMM;  { Lui imm }
  | LLI  ;          imm=IMM;  { Lli imm }
  | LI   ;     imm=IMM     ;  { Li  imm }
  | LA   ;     lbl=LABEL   ;  { La  lbl }

  | MOV  ; rs1=REG; rs2=REG;  { Move  (rs1, rs2) }
  | ADD  ; rs1=REG; rs2=REG;  { Add   (rs1, rs2) }
  | SUB  ; rs1=REG; rs2=REG;  { Sub   (rs1, rs2) }
  | AND  ; rs1=REG; rs2=REG;  { And   (rs1, rs2) }
  | OR   ; rs1=REG; rs2=REG;  { Or    (rs1, rs2) }
  | NOT  ; rs1=REG         ;  { Not    rs1       }

  | BEQZ ; rs1=REG; rs2=REG;  { Beqz  (rs1, rs2) }
  | BLTZ ; rs1=REG; rs2=REG;  { Bltz  (rs1, rs2) }
  | BGEZ ; rs1=REG; rs2=REG;  { Bgez  (rs1, rs2) }
  | BGTZ ; rs1=REG; rs2=REG;  { Bgtz  (rs1, rs2) }
  | BLEZ ; rs1=REG; rs2=REG;  { Blez  (rs1, rs2) }
  | BNEZ ; rs1=REG; rs2=REG;  { Bnez  (rs1, rs2) }

  | JUMP ;          rs2=REG;  { Jump        rs2  }
  | STPC ; rs1=REG         ;  { StPC   rs1       }

  | lbl=LABEL; COLON       ;  { Label lbl }
