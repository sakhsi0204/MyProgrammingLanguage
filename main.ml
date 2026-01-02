open Lexing
open Ast
open Type_checker  (* Assuming your type checking functions are in a module named Type_checker *)
open Interpreter   (* Assuming your interpreter code is in a module named Interpreter *)

let () =
  (* Choose input: either from a file or from stdin *)
  let in_channel =
    if Array.length Sys.argv > 1 then 
      open_in Sys.argv.(1)
    else 
      stdin 
  in
  let lexbuf = from_channel in_channel in
  try
    let ast = Parser.program Lexer.token lexbuf in
    Printf.printf "Parsing successful.\n";

    (* Type checking phase *)
    Type_checker.type_check_stmt ast Ast.tau;
    Printf.printf "Type checking successful.\n";

    (* Evaluation phase *)
    Printf.printf "Evaluating program...\n";
    Interpreter.eval_program ast;
    Printf.printf "Evaluation complete.\n"
  with
  | Lexer.Error msg ->
      Printf.eprintf "Lexing failed: %s\n" msg;
      exit 1
  | Parsing.Parse_error ->
      Printf.eprintf "Parsing failed: Syntax error.\n";
      exit 1
  | Type_checker.Type_error msg ->
      Printf.eprintf "Type checking failed: %s\n" msg;
      exit 1
  | e ->
      Printf.eprintf "An unexpected error occurred: %s\n" (Printexc.to_string e);
      exit 1
