
make:
	
	cp lexer/lexer.mll out/
	cp parser/parser.mly out/
	cp compiler/compiler.ml out/
	cp compiler/mc.ml out/
    
	cd out/; \
	ocamllex lexer.mll; \
	menhir --explain parser.mly; \
	ocamlfind ocamlc -c mc.ml -linkpkg \
  	-package spelll; \
	ocamlfind ocamlc -c parser.mli -linkpkg \
  	-package spelll; \
	ocamlfind ocamlc -c lexer.ml -linkpkg \
  	-package spelll; \
	ocamlfind ocamlc -c parser.ml -linkpkg \
  	-package spelll; \
	ocamlfind ocamlc -c compiler.ml -linkpkg \
  	-package spelll; \
	ocamlfind ocamlc -o ../Mcc mc.cmo lexer.cmo parser.cmo compiler.cmo -linkpkg \
  	-package spelll 




clean:
	rm -f out/*
	