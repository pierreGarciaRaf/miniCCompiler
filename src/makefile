
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
	ocamlc -c calc.ml; \
	ocamlc -o ../calc lexer.cmo parser.cmo calc.cmo
	

clean:
	rm out/*

