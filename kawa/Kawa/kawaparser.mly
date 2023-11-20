%{

  open Lexing
  open Kawa

%}

%token THIS
%token <int> INT
%token <string> IDENT
%token <bool> BOOL
%token TRUE FALSE
%token VOID
%token ADD SUB MUL DIV EQU
%token NOT OPP
%token LT LE GT GE 
%token AND OR 
%token ATTRIBUTE VAR CLASS METHOD
%token MAIN
%token IF ELSE WHILE
%token LPAR RPAR BEGIN END SEMI
%token PRINT RETURN NEW
%token EOF

%start program
%type <Kawa.program> program

%%

program:
| MAIN BEGIN main=list(instruction) END EOF
    { {classes=[]; globals=[]; main} }
;

instruction:
| PRINT LPAR e=expression RPAR SEMI { Print(e) }
| RETURN e=expression SEMI { Return(e) }
;

expression:
| n=INT { Int(n) } 
| b=BOOL{ Bool(b)}
| e1=expression op=binop e2=expression { Binop(op,e1,e2) }
| up=unop e1=expression {Unop(up,e1)}
;



%inline binop:
  | ADD  { Add } (* + *)
  | MUL   { Mul } (* * *)
  | SUB { Sub } (* - *)
  | DIV   { Div } (* / *)
  | MOD   { Rem } (* mod *)
  | EQU    { Eq } (* == *)
  | NEQ   { Neq } (* != *)
  | LT    { Lt } (* < *)
  | LE    { Le } (* <= *)
  | GT    { Gt } (* > *)
  | GE    { Ge } (* >= *)
  | AND   { And } (* && *)
  | OR    { Or } (* || *)
;



%inline   unop: 
| MINUS { Neg } 
| NOT { Not } 
;