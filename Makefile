CC=ocamlc
CFLAGS=-c -g

all: rustine

ast.cmi: ast.mli
	$(CC) $(CFLAGS) ast.mli

ast.cmo: ast.ml ast.cmi
	$(CC) $(CFLAGS) ast.ml

lexer.ml: lexer.mll
	ocamllex lexer.mll

parser.ml parser.mli: parser.mly
	ocamlyacc parser.mly

parser.cmi: parser.mli ast.cmi
	$(CC) $(CFLAGS) parser.mli

parser.cmo: parser.ml parser.cmi ast.cmo
	$(CC) $(CFLAGS) parser.ml

lexer.cmo: lexer.ml parser.cmi
	$(CC) $(CFLAGS) lexer.ml

main.cmo: main.ml parser.cmi lexer.cmi ast.cmo
	$(CC) $(CFLAGS) main.ml

rustine: ast.cmo parser.cmo lexer.cmo main.cmo
	$(CC) -o rustine ast.cmo parser.cmo lexer.cmo main.cmo

clean:
	rm -f *.cmo *.cmi rustine lexer.ml parser.ml parser.mli
