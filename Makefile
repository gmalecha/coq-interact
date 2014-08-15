native:
	ocamlbuild -use-ocamlfind -I src test.native

byte:
	ocamlbuild -use-ocamlfind -I src test.byte
