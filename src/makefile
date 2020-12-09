
make:
	
	cp lexer/lexer.mll out/
	cp parser/parser.mly out/
	cp compiler/compiler.ml out/
	cp compiler/mc.ml out/
    
	cd out/; \
	ocamllex lexer.mll; \
	menhir parser.mly; \
	ocamlc -c mc.ml; \
	ocamlc -c parser.mli; \
	ocamlc -c lexer.ml; \
	ocamlc -c parser.ml; \
	ocamlc -c compiler.ml; \
	ocamlc -o ../Mcc mc.cmo lexer.cmo parser.cmo compiler.cmo 




clean:
	rm -f out/*
	