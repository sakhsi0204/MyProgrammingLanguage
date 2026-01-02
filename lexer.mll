{
    open Parser
    exception Error of string
}
(* type token = 
| INPUT | PRINT
| IDENTIFIER of string 
| FILENAME of string
| INT of int | FLOAT of float | BOOL of bool 
| NOT | AND | OR 
| PLUS | MINUS | TIMES | DIVIDE | ABS 
| EQ | NEQ | LT | GT | LEQ | GEQ | MOD 
| LBRACKET | RBRACKET | LBRACE | RBRACE | LPAREN | RPAREN
| COMMA | SEMICOLON 
| DOTPRODUCT | ANGLE | MAG | DIM | UNIT | CREATE | ZERO
| MULMATRIX | TRANSPOSE | DETERMINANT | INVERT
| ASSIGN 
| IF | ELSE 
| FOR | WHILE 
| EOF
| ROWSWAP
| ROWREDUCE


*)
(* Regular expressions *)
let digit = ['0'-'9']
let letter = ['a'-'z' 'A'-'Z']
let identifier = letter (letter|digit|'_'|'\'')*
let filename = (letter|digit|'_'|'/')+'.'(letter)+
let number = ['-']? digit+
let floatnumber = ['-']? digit*'.'digit+ 

(* Token definitions *)

rule token = parse
| [' ' '\t' '\n' '\r'] { token lexbuf }
| "(*" { comment lexbuf }
| "Input" { INPUT }
| "Print" { PRINT }
| "raise" { RAISE }
| number as n { INT (int_of_string n) }
| floatnumber as f { FLOAT (float_of_string f) }
| "true" { BOOL true }
| "false" { BOOL false }
| "if" { IF }
| "else" { ELSE }
| "for" { FOR }
| "while" { WHILE }

| "not" { NOT }
| "&" { AND }
| "|" { OR }

| "+" { PLUS }
| "-" { MINUS }
| "*" { TIMES }
| "/" { DIVIDE }
| "%" { MOD }
| "abs" { ABS }

| "=" { EQ }
| "!=" { NEQ }
| "<" { LT }
| ">" { GT }
| "<=" { LEQ }
| ">=" { GEQ }

| "(" { LPAREN }
| ")" { RPAREN }
| "{" { LBRACE }
| "}" { RBRACE }
| "[" { LBRACKET }
| "]" { RBRACKET }
| "," { COMMA }
| ";" { SEMICOLON }

| "<:" { ANGLE }
| "dim" { DIM }
| "unit" { UNIT }
| "create" { CREATE}
| "iszero"  { ZERO }
| "mag" { MAG }
| "." { DOTPRODUCT }
(* new *)
| "minor" { MINOR }
| "power" { POWER }
| "rows" { ROWS }
| "cols" { COLS }
| "size" { SIZE }

| "**" { MULMATRIX}
| "^" { TRANSPOSE }
| "det" { DETERMINANT }
|":=" { ASSIGN }
| "row_swap" { ROWSWAP }
| "row_reduce" { ROWREDUCE }

| identifier as id { IDENTIFIER id }
| filename as fn { FILENAME fn }

| eof { EOF }
| _ { raise (Error "Illegal character") }

and comment = parse
| "*)"                     { token lexbuf }
| eof                      { failwith "Unterminated comment" }
| _                        { comment lexbuf }

