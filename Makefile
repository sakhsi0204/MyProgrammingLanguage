all: 
	@ocamlc -c ast.ml
	@ocamlc -c token.ml
	@ocamlyacc parser.mly
	@ocamlc -c parser.mli
	@ocamlc -c parser.ml
	@ocamllex lexer.mll
	@ocamlc -c lexer.ml
	@ocamlc -c type_checker.ml
	@ocamlc -c interpreter.ml
	@ocamlc -c main.ml
	@ocamlc -o my_program ast.cmo lexer.cmo parser.cmo type_checker.cmo interpreter.cmo main.cmo 

test:
	@./my_program temp.txt

clean:
	@rm -f *.cmi *.cmo *.cmx lexer.ml parser.ml parser.mli *.out *my_program parser.output
