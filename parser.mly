%{
    open Ast
%}

%token INPUT PRINT
%token <string> IDENTIFIER
%token <string> FILENAME
%token <int> INT
%token <float> FLOAT
%token <bool> BOOL
%token NOT AND OR
%token PLUS MINUS TIMES DIVIDE ABS
%token EQ NEQ LT GT LEQ GEQ MOD
%token LBRACKET RBRACKET LBRACE RBRACE LPAREN RPAREN
%token COMMA SEMICOLON
%token DOTPRODUCT ANGLE MAG DIM UNIT CREATE ZERO
%token MULMATRIX TRANSPOSE DETERMINANT
%token POWER MINOR SIZE COLS ROWS
%token ASSIGN
%token IF ELSE
%token FOR WHILE
%token EOF
%token ROWSWAP ROWREDUCE
%token RAISE

%left OR
%left AND
%left EQ NEQ LT GT LEQ GEQ
%left PLUS MINUS
%left TIMES DIVIDE MOD POWER 
%left ANGLE DOTPRODUCT MULMATRIX 
%right NOT ABS TRANSPOSE MAG DIM DETERMINANT MINOR SIZE COLS ROWS

%start program
%type <Ast.stmt> program
%type <Ast.stmt list> stmt_list
%type <Ast.stmt> stmt
%type <Ast.expr> expr
%type <Ast.expr list> expr_list
%type <Ast.expr list> vector
%type <Ast.expr list list> matrix_list
%type <Ast.expr list list> matrix
%%

program:
  stmt_list EOF { Seq($1) }
;

stmt_list:
    /* empty */ { [] }
  | stmt stmt_list { $1 :: $2 }
;

stmt:

     IDENTIFIER ASSIGN expr SEMICOLON 
       { Assign($1, $3) }
  | IDENTIFIER LBRACKET expr RBRACKET ASSIGN expr SEMICOLON
       { AssignVec($1,$3,$6) }
  | IDENTIFIER LBRACKET expr RBRACKET LBRACKET expr RBRACKET ASSIGN expr SEMICOLON
           { AssignMat($1,$3,$6,$9) }
  | PRINT LPAREN expr RPAREN SEMICOLON 
       { Print($3) }
  | IF LPAREN expr RPAREN stmt ELSE stmt 
       { If_Then_Else($3, $5, $7) }
  | FOR LPAREN stmt stmt expr RPAREN stmt
       { For($3, $4, $5, $7) }
  | WHILE LPAREN expr RPAREN stmt 
       { While($3, $5) }
  | LBRACE stmt_list RBRACE 
       { Block($2) }
  | 
  | RAISE IDENTIFIER SEMICOLON
     {Raise($2)}
;

expr:
    | IDENTIFIER LBRACKET expr RBRACKET
        { GetVec($1,$3) }
    | IDENTIFIER LBRACKET expr RBRACKET LBRACKET expr RBRACKET
        { Getmat($1,$3,$6) }
  | INPUT LPAREN FILENAME RPAREN  
       { Input(Some($3)) }
  | INPUT LPAREN RPAREN  
       { Input(None) }
  | INPUT LPAREN INT COMMA FILENAME RPAREN  
       { Inputvec($3, Some($5)) }
| INPUT LPAREN INT RPAREN  
     { Inputvec($3, None) }
| INPUT LPAREN INT COMMA INT COMMA FILENAME RPAREN 
     { InputMat($3,$5,Some($7)) }
| INPUT LPAREN INT COMMA INT RPAREN 
     { InputMat($3,$5,None) }
  | INT 
       { Int($1) }
  | FLOAT 
       { Float($1) }
  | BOOL 
       { Bool($1) }
  | IDENTIFIER 
       { Id($1) }
  | vector 
       { Vector($1) }
  | matrix
         { Matrix($1) }
  | LPAREN expr RPAREN 
       { $2 }
  | expr PLUS expr 
       { Binop($1, Plus, $3) }
  | expr MINUS expr 
       { Binop($1, Minus, $3) }
  | expr TIMES expr 
       { Binop($1, Times, $3) }
  | expr DIVIDE expr 
       { Binop($1, Divide, $3) }
  | expr MOD expr 
       { Binop($1, Mod, $3) }
  | expr EQ expr 
       { Binop($1, Eq, $3) }
  | expr NEQ expr 
       { Binop($1, Neq, $3) }
  | expr LT expr 
       { Binop($1, Lt, $3) }
  | expr GT expr 
       { Binop($1, Gt, $3) }
  | expr LEQ expr 
       { Binop($1, Leq, $3) }
  | expr GEQ expr 
       { Binop($1, Geq, $3) }
  | expr AND expr 
       { Binop($1, And, $3) }
  | expr OR expr 
       { Binop($1, Or, $3) }
  | expr ANGLE expr 
       { Binop($1, Angle, $3) }
  | expr DOTPRODUCT expr 
       { Binop($1, Dotproduct, $3) }
  | expr MULMATRIX expr 
       { Binop($1, Mulmatrix, $3) }
  | expr POWER expr 
       { Binop($1, Power, $3) }
  
  | NOT expr 
       { Unop(Not, $2) }
  | ABS expr 
       { Unop(Abs, $2) } 
  |  expr TRANSPOSE
       { Unop(Transpose, $1) }
  | SIZE LPAREN expr RBRACE
       { Unop(Size, $3) }
  | ROWS LBRACE expr RBRACE
       { Unop(Rows, $3) }
  | COLS LBRACE expr RBRACE
       { Unop(Cols, $3) }
  | MAG expr 
       { Unop(Mag, $2) }
  | DIM expr 
       { Unop(Dim, $2) }
    | DETERMINANT expr
       { Unop(Determinant, $2) }
    | MINOR LPAREN IDENTIFIER COMMA expr COMMA expr RPAREN
       { Minor($3,$5,$7) }
    | CREATE LPAREN INT COMMA expr RPAREN
       { CreateVec($3,$5) }
    | CREATE LPAREN INT COMMA INT COMMA expr RPAREN
       { CreateMat($3,$5,$7) }
    | ROWSWAP LPAREN expr COMMA INT COMMA INT RPAREN 
        { Rowswap($3, $5, $7) }
    | ROWREDUCE LPAREN expr COMMA INT COMMA FLOAT COMMA INT RPAREN
        { Rowreduce($3,$5,$7,$9) }    //R2-> R2 + alpha R1 ----> reduce(matrix,2,alpha,1)
    | ZERO LPAREN expr RPAREN
        { Iszero($3) }
    | UNIT LPAREN INT COMMA INT RPAREN
        { Unit($3,$5) }
;

expr_list:
    expr 
       { [$1] }
  | expr COMMA expr_list
       { $1 :: $3 }
;
vector :
    LBRACKET expr_list RBRACKET 
       { $2 }
;

matrix_list:
    vector 
       { [$1] }
  | vector COMMA matrix_list 
       { $1 :: $3 }
;

matrix:
    LBRACE matrix_list RBRACE 
       { $2 }
;