tortu:
	ocamlbuild -use-ocamlfind src/tortu.byte

clean:
	ocamlbuild -clean

.PHONY: tortu
