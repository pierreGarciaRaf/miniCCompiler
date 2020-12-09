
make:
	
	cp lexer/lexer.mll out/
	cp parser/parser.mly out/
	cp compiler/calc.ml out/
    
	cd out/; \
	ocamllex lexer.mll; \
	ocamlyacc parser.mly; \
	ocamlc -c parser.mli; \
	ocamlc -c lexer.ml; \
	ocamlc -c parser.ml; \
	ocamlc -c compiler.ml; \
	ocamlc -o ../Mc++ lexer.cmo parser.cmo compiler.cmo
	

clean:
	rm out/*

