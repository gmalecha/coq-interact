byte:
	ocamlbuild -use-ocamlfind -I src test.byte

native:
	ocamlbuild -use-ocamlfind -I src test.native

clean:
	ocamlbuild -use-ocamlfind -clean
