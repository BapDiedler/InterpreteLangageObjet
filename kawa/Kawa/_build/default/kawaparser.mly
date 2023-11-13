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
;